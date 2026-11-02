! A habit tracker written in Fortran, including the web server. Every habit is a punch card
! with 80 columns, one per day. Doing the habit punches a hole in today's column, in the
! row that matches the last digit of the date, so a streak shows up as a staircase.
! No JavaScript: every click is a plain form post.
!
!   gfortran -O2 habits.f90 -o habits
!   ./habits            then open http://localhost:8080

! ---- libc, called directly through the standard C interop ------------------------------
module sockets
  use iso_c_binding
  implicit none

  ! macOS layout of struct sockaddr_in. Linux has no sin_len byte.
  type, bind(c) :: sockaddr_in
    integer(c_int8_t)  :: sin_len, sin_family
    integer(c_int16_t) :: sin_port
    integer(c_int32_t) :: sin_addr
    integer(c_int8_t)  :: sin_zero(8)
  end type

  integer(c_int), parameter :: AF_INET = 2, SOCK_STREAM = 1
  integer(c_int), parameter :: SOL_SOCKET = 65535, SO_REUSEADDR = 4  ! macOS values
  integer(c_int), parameter :: SIGCHLD = 20

  interface
    integer(c_int) function c_socket(domain, type, protocol) bind(c, name="socket")
      import; integer(c_int), value :: domain, type, protocol
    end function
    integer(c_int) function c_setsockopt(fd, level, name, val, len) bind(c, name="setsockopt")
      import; integer(c_int), value :: fd, level, name, len
      integer(c_int), intent(in) :: val
    end function
    integer(c_int) function c_bind(fd, addr, len) bind(c, name="bind")
      import; integer(c_int), value :: fd, len
      type(sockaddr_in), intent(in) :: addr
    end function
    integer(c_int) function c_listen(fd, backlog) bind(c, name="listen")
      import; integer(c_int), value :: fd, backlog
    end function
    integer(c_int) function c_accept(fd, addr, len) bind(c, name="accept")
      import; integer(c_int), value :: fd
      type(c_ptr), value :: addr, len
    end function
    integer(c_size_t) function c_read(fd, buf, n) bind(c, name="read")
      import; integer(c_int), value :: fd
      character(kind=c_char), intent(out) :: buf(*)
      integer(c_size_t), value :: n
    end function
    integer(c_size_t) function c_write(fd, buf, n) bind(c, name="write")
      import; integer(c_int), value :: fd
      character(kind=c_char), intent(in) :: buf(*)
      integer(c_size_t), value :: n
    end function
    integer(c_int) function c_close(fd) bind(c, name="close")
      import; integer(c_int), value :: fd
    end function
    integer(c_int) function c_fork() bind(c, name="fork")
      import
    end function
    integer(c_int) function c_alarm(seconds) bind(c, name="alarm")
      import; integer(c_int), value :: seconds
    end function
    integer(c_intptr_t) function c_signal(sig, handler) bind(c, name="signal")
      import; integer(c_int), value :: sig
      integer(c_intptr_t), value :: handler
    end function
    integer(c_int) function c_rename(from, to) bind(c, name="rename")
      import; character(kind=c_char), intent(in) :: from(*), to(*)
    end function
  end interface
end module

! ---- dates: whole days since long ago, so "yesterday" is just minus one -----------------
module dates
  implicit none
  character(len=3), parameter :: MONTHS(12) = &
    ['JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC']
contains
  integer function day_number(y, m, d) result(n)
    integer, intent(in) :: y, m, d
    integer :: a, yy, mm
    a = (14 - m) / 12
    yy = y + 4800 - a
    mm = m + 12 * a - 3
    n = d + (153 * mm + 2) / 5 + 365 * yy + yy / 4 - yy / 100 + yy / 400 - 32045
  end function

  subroutine from_day_number(n, y, m, d)
    integer, intent(in) :: n
    integer, intent(out) :: y, m, d
    integer :: a, b, c, e, f, g
    a = n + 32044
    b = (4 * a + 3) / 146097
    c = a - 146097 * b / 4
    e = (4 * c + 3) / 1461
    f = c - 1461 * e / 4
    g = (5 * f + 2) / 153
    d = f - (153 * g + 2) / 5 + 1
    m = g + 3 - 12 * (g / 10)
    y = 100 * b + e - 4800 + g / 10
  end subroutine

  integer function today() result(n)
    integer :: v(8)
    call date_and_time(values=v)
    n = day_number(v(1), v(2), v(3))
  end function
end module

! ---- the database: two text files with fixed-width lines --------------------------------
module store
  use iso_c_binding, only: c_null_char
  use sockets, only: c_rename
  use dates
  implicit none
  integer, parameter :: NAME_LEN = 40, MAX_HABITS = 12, CARD_COLUMNS = 80
  character(len=*), parameter :: HABITS_FILE = 'data/habits.dat', PUNCHES_FILE = 'data/punches.dat'

  type :: habit
    integer :: id, created
    character(len=NAME_LEN) :: name
  end type

  type :: punch
    integer :: habit_id, day
  end type

  character(len=*), parameter :: HABIT_FMT = '(i4,1x,i8,1x,a)', PUNCH_FMT = '(i4,1x,i8)'
contains

  integer function count_lines(path) result(n)
    character(len=*), intent(in) :: path
    integer :: u, ios
    character(len=1) :: c
    n = 0
    open(newunit=u, file=path, status='old', action='read', iostat=ios)
    if (ios /= 0) return
    do
      read(u, '(a)', iostat=ios) c
      if (ios /= 0) exit
      n = n + 1
    end do
    close(u)
  end function

  subroutine load_habits(h)
    type(habit), allocatable, intent(out) :: h(:)
    integer :: u, ios, i, n
    n = count_lines(HABITS_FILE)
    allocate(h(n))
    if (n == 0) return
    open(newunit=u, file=HABITS_FILE, status='old', action='read')
    do i = 1, n
      read(u, HABIT_FMT, iostat=ios) h(i)%id, h(i)%created, h(i)%name
    end do
    close(u)
  end subroutine

  subroutine load_punches(p)
    type(punch), allocatable, intent(out) :: p(:)
    integer :: u, ios, i, n
    n = count_lines(PUNCHES_FILE)
    allocate(p(n))
    if (n == 0) return
    open(newunit=u, file=PUNCHES_FILE, status='old', action='read')
    do i = 1, n
      read(u, PUNCH_FMT, iostat=ios) p(i)%habit_id, p(i)%day
    end do
    close(u)
  end subroutine

  ! Write to a temporary file, then rename: a crash can't leave half a database behind.
  subroutine save_habits(h)
    type(habit), intent(in) :: h(:)
    integer :: u, i, ignored
    open(newunit=u, file=HABITS_FILE // '.tmp', status='replace', action='write')
    do i = 1, size(h)
      write(u, HABIT_FMT) h(i)%id, h(i)%created, trim(h(i)%name)
    end do
    close(u)
    ignored = c_rename(HABITS_FILE // '.tmp' // c_null_char, HABITS_FILE // c_null_char)
  end subroutine

  subroutine save_punches(p)
    type(punch), intent(in) :: p(:)
    integer :: u, i, ignored
    open(newunit=u, file=PUNCHES_FILE // '.tmp', status='replace', action='write')
    do i = 1, size(p)
      write(u, PUNCH_FMT) p(i)%habit_id, p(i)%day
    end do
    close(u)
    ignored = c_rename(PUNCHES_FILE // '.tmp' // c_null_char, PUNCHES_FILE // c_null_char)
  end subroutine

  logical function punched(p, id, day)
    type(punch), intent(in) :: p(:)
    integer, intent(in) :: id, day
    punched = any(p%habit_id == id .and. p%day == day)
  end function

  ! Today still counts as open: a streak only breaks once a whole day was missed.
  integer function streak(p, id) result(n)
    type(punch), intent(in) :: p(:)
    integer, intent(in) :: id
    integer :: d
    n = 0
    d = today()
    if (.not. punched(p, id, d)) d = d - 1
    do while (punched(p, id, d))
      n = n + 1
      d = d - 1
    end do
  end function

  logical function toggle_punch(id, day) result(now_punched)
    integer, intent(in) :: id, day
    type(punch), allocatable :: p(:)
    call load_punches(p)
    now_punched = .not. punched(p, id, day)
    if (now_punched) then
      p = [p, punch(id, day)]
    else
      p = pack(p, .not. (p%habit_id == id .and. p%day == day))
    end if
    call save_punches(p)
  end function
end module

! ---- HTTP and HTML ----------------------------------------------------------------------
module web
  use iso_c_binding, only: c_int, c_size_t
  use sockets
  use dates
  use store
  implicit none
  character(len=2), parameter :: CRLF = char(13) // char(10)
  character(len=:), allocatable :: page_buf
  integer :: page_len = 0

  character(len=*), parameter :: CSS = &
    'body{background:#d9cba6;color:#2b2418;font:15px/1.4 ui-monospace,Menlo,monospace;margin:0;padding:28px 16px}' // &
    'main{width:1140px;margin:0 auto}h1{font-size:24px;letter-spacing:4px;margin:0}' // &
    '.sub{margin:2px 0 22px;color:#6b5c3e}' // &
    '.shadow{filter:drop-shadow(0 3px 5px rgba(60,45,20,.45));margin-bottom:24px;position:relative}' // &
    '.card{background:#f4e8ca;padding:12px 24px 6px;clip-path:polygon(26px 0,100% 0,100% 100%,0 100%,0 26px)}' // &
    '.top{display:flex;align-items:center;gap:18px;height:50px}' // &
    '.name{font-size:24px;font-weight:700;letter-spacing:2px;flex:1;overflow:hidden;white-space:nowrap}' // &
    '.stats{color:#6b5c3e;white-space:nowrap}.stats b{color:#2b2418}' // &
    '.lever{font:700 14px ui-monospace,Menlo,monospace;letter-spacing:1px;padding:9px 16px;background:#2b2418;' // &
    'color:#f4e8ca;border:0;border-radius:3px;cursor:pointer}.lever:hover{background:#a3271c}' // &
    '.lever:active{transform:translateY(2px)}.lever.undo{background:none;color:#6b5c3e;box-shadow:inset 0 0 0 2px #b7a27a}' // &
    '.grid{display:flex;margin-top:14px}.c{width:13.6px;position:relative;text-align:center}' // &
    '.c.t{background:rgba(163,39,28,.13)}.c i,.c button{display:block;height:21px;line-height:21px;width:100%;' // &
    'padding:0;border:0;background:none;font:400 11px/21px ui-monospace,Menlo,monospace;color:#a8926a;font-style:normal}' // &
    '.c.f i{color:#d3c29c}.c button{cursor:pointer}.c button:hover{outline:2px solid #a3271c;outline-offset:-2px;color:#a3271c}' // &
    '.c button.p{color:transparent;position:relative}' // &
    '.c button.p::after{content:"";position:absolute;left:3.3px;right:3.3px;top:2px;bottom:2px;background:#2b2418;' // &
    'border-radius:1.5px;box-shadow:inset 0 1px 2px #000}' // &
    '.c button.new::after{animation:pop .35s ease-out}@keyframes pop{0%{transform:scale(3);opacity:0}100%{transform:scale(1)}}' // &
    '.lab{position:absolute;left:50%;transform:translateX(-50%);top:-15px;font-size:9px;font-weight:700;color:#6b5c3e;white-space:nowrap}' // &
    '.lab.now{color:#a3271c}.nums{display:flex;margin-top:3px;font-size:6.5px;color:#a8926a}.nums span{width:13.6px;text-align:center}' // &
    '.stamp{position:absolute;top:88px;white-space:nowrap;padding:3px 14px;border:4px solid #a3271c;border-radius:4px;color:#a3271c;' // &
    'font:700 26px "Courier New",Courier,monospace;transform:rotate(-7deg);opacity:.82;pointer-events:none;background:rgba(244,232,202,.35)}' // &
    '.stamp.thump{animation:thump .4s cubic-bezier(.2,1.6,.5,1)}@keyframes thump{0%{transform:rotate(-7deg) scale(2.4);opacity:0}100%{transform:rotate(-7deg) scale(1);opacity:.82}}' // &
    'details{font-size:12px;color:#6b5c3e}summary{cursor:pointer;list-style:none}summary:hover{color:#a3271c}' // &
    'details button{font:12px ui-monospace,Menlo,monospace;color:#a3271c;background:none;border:0;text-decoration:underline;cursor:pointer;padding:0}' // &
    '.new{display:flex;gap:10px;margin-bottom:26px}.new input{font:700 15px ui-monospace,Menlo,monospace;text-transform:uppercase;letter-spacing:1px;' // &
    'padding:9px 12px;width:440px;background:#f4e8ca;border:2px solid #2b2418;border-radius:2px;color:#2b2418}' // &
    '.empty{padding:30px 0;text-align:center;color:#6b5c3e}.help{margin-top:22px;color:#6b5c3e;font-size:13px}' // &
    '@media (prefers-reduced-motion:reduce){*{animation:none!important}}'

contains

  subroutine add(s)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: bigger
    if (.not. allocated(page_buf)) allocate(character(len=65536) :: page_buf)
    if (page_len + len(s) > len(page_buf)) then
      allocate(character(len=2 * (page_len + len(s))) :: bigger)
      bigger(1:page_len) = page_buf(1:page_len)
      call move_alloc(bigger, page_buf)
    end if
    page_buf(page_len + 1:page_len + len(s)) = s
    page_len = page_len + len(s)
  end subroutine

  function str(v) result(s)
    integer, intent(in) :: v
    character(len=:), allocatable :: s
    character(len=16) :: b
    write(b, '(i0)') v
    s = trim(b)
  end function

  function escape_html(s) result(out)
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: out
    integer :: i
    out = ''
    do i = 1, len_trim(s)
      select case (s(i:i))
      case ('&'); out = out // '&amp;'
      case ('<'); out = out // '&lt;'
      case ('>'); out = out // '&gt;'
      case ('"'); out = out // '&quot;'
      case (''''); out = out // '&#39;'
      case default; out = out // s(i:i)
      end select
    end do
  end function

  ! Value of name= in a form body, with + and %XX decoded.
  function form_value(body, name) result(out)
    character(len=*), intent(in) :: body, name
    character(len=:), allocatable :: out, amp
    character(len=*), parameter :: HEX = '0123456789abcdef'
    integer :: at, e, i, a, b
    character(len=1) :: ch

    out = ''
    amp = '&' // body // '&'
    at = index(amp, '&' // name // '=')
    if (at == 0) return
    at = at + len(name) + 2
    e = index(amp(at:), '&') + at - 2
    i = at
    do while (i <= e)
      ch = amp(i:i)
      if (ch == '+') then
        ch = ' '
      else if (ch == '%' .and. i + 2 <= e) then
        a = index(HEX, lower(amp(i + 1:i + 1))); b = index(HEX, lower(amp(i + 2:i + 2)))
        if (a > 0 .and. b > 0) then
          ch = char((a - 1) * 16 + b - 1)
          i = i + 2
        end if
      end if
      out = out // ch
      i = i + 1
    end do
  end function

  pure function lower(c) result(l)
    character(len=1), intent(in) :: c
    character(len=1) :: l
    l = c
    if (c >= 'A' .and. c <= 'Z') l = char(ichar(c) + 32)
  end function

  integer function to_int(s) result(v)
    character(len=*), intent(in) :: s
    integer :: ios
    v = -1
    if (len_trim(s) == 0 .or. verify(trim(s), '0123456789') /= 0 .or. len_trim(s) > 8) return
    read(s, *, iostat=ios) v
    if (ios /= 0) v = -1
  end function

  ! Keypunches only had capitals. Bytes above 127 (UTF-8) pass through untouched.
  function card_name(raw) result(name)
    character(len=*), intent(in) :: raw
    character(len=NAME_LEN) :: name
    integer :: i, n, v
    name = ''
    n = 0
    do i = 1, len(raw)
      v = ichar(raw(i:i))
      if (v < 32 .or. v == 127) cycle
      if (n == NAME_LEN) exit
      n = n + 1
      name(n:n) = raw(i:i)
      if (name(n:n) >= 'a' .and. name(n:n) <= 'z') name(n:n) = char(v - 32)
    end do
    ! Don't leave half a multi-byte character at the cut.
    if (n == NAME_LEN) then
      do while (n > 0 .and. ichar(name(n:n)) >= 128 .and. ichar(name(n:n)) < 192)
        name(n:n) = ' '
        n = n - 1
      end do
      if (n > 0 .and. ichar(name(n:n)) >= 192) name(n:n) = ' '
    end if
    name = adjustl(name)
  end function

  subroutine send(fd, data)
    integer(c_int), intent(in) :: fd
    character(len=*), intent(in) :: data
    integer(c_size_t) :: n
    integer :: p
    p = 1
    do while (p <= len(data))
      n = c_write(fd, data(p:), int(len(data) - p + 1, c_size_t))
      if (n <= 0 .or. n > len(data)) return
      p = p + int(n)
    end do
  end subroutine

  subroutine respond(fd, status, ctype, body, extra)
    integer(c_int), intent(in) :: fd
    character(len=*), intent(in) :: status, ctype, body
    character(len=*), intent(in), optional :: extra
    character(len=:), allocatable :: head
    head = 'HTTP/1.1 ' // status // CRLF // 'Content-Type: ' // ctype // CRLF // &
           'Content-Length: ' // str(len(body)) // CRLF // 'Connection: close' // CRLF
    if (present(extra)) head = head // extra // CRLF
    call send(fd, head // CRLF)
    call send(fd, body)
  end subroutine

  subroutine render_card(h, p, now, fresh_day, stamp_it)
    type(habit), intent(in) :: h
    type(punch), intent(in) :: p(:)
    integer, intent(in) :: now, fresh_day
    logical, intent(in) :: stamp_it
    integer :: card_no, first, col, day, row, y, m, d, hole, total
    logical :: done_today, is_hole
    character(len=16) :: date_text, stamp_pos
    real :: stamp_x
    character(len=:), allocatable :: id

    id = str(h%id)
    card_no = (now - h%created) / CARD_COLUMNS + 1
    first = h%created + (card_no - 1) * CARD_COLUMNS
    done_today = punched(p, h%id, now)
    total = count(p%habit_id == h%id)

    call add('<div class="shadow"><form method="post" action="/punch" class="card"><input type="hidden" name="h" value="' // id // '">')
    call add('<div class="top"><div class="name">' // escape_html(h%name) // '</div><div class="stats">STREAK <b>' // &
             str(streak(p, h%id)) // '</b> &middot; TOTAL <b>' // str(total) // '</b> &middot; CARD <b>' // str(card_no) // '</b></div>')
    if (done_today) then
      call add('<button class="lever undo" name="d" value="' // str(now) // '">UNDO TODAY</button>')
    else
      call add('<button class="lever" name="d" value="' // str(now) // '">PUNCH TODAY</button>')
    end if
    call add('</div><div class="grid">')

    do col = 1, CARD_COLUMNS
      day = first + col - 1
      call from_day_number(day, y, m, d)
      hole = mod(d, 10)
      write(date_text, '(i4.4,"-",i2.2,"-",i2.2)') y, m, d
      if (day == now) then
        call add('<div class="c t"><span class="lab now">TODAY</span>')
      else if (day > now) then
        call add('<div class="c f">')
      else
        call add('<div class="c">')
      end if
      if (d == 1 .and. abs(day - now) > 3) call add('<span class="lab">' // MONTHS(m) // '</span>')
      do row = 0, 9
        is_hole = row == hole .and. day <= now
        if (is_hole) then
          if (punched(p, h%id, day)) then
            if (day == fresh_day) then
              call add('<button class="p new" name="d" value="' // str(day) // '" title="' // trim(date_text) // '">' // str(row) // '</button>')
            else
              call add('<button class="p" name="d" value="' // str(day) // '" title="' // trim(date_text) // '">' // str(row) // '</button>')
            end if
          else
            call add('<button name="d" value="' // str(day) // '" title="' // trim(date_text) // '">' // str(row) // '</button>')
          end if
        else
          call add('<i>' // str(row) // '</i>')
        end if
      end do
      call add('</div>')
    end do

    call add('</div><div class="nums">')
    do col = 1, CARD_COLUMNS
      call add('<span>' // str(col) // '</span>')
    end do
    call add('</div></form>')

    if (done_today) then
      call from_day_number(now, y, m, d)
      write(date_text, '(i2.2,1x,a3)') d, MONTHS(m)
      ! Stamp beside today's column, on the blank days, or on the old days once it's near the edge.
      col = now - first + 1
      stamp_x = 24 + col * 13.6 + 12
      if (col > 58) stamp_x = stamp_x - 13.6 - 262
      write(stamp_pos, '(i0)') nint(stamp_x)
      call add('<div class="stamp' // merge(' thump', '      ', stamp_it) // '" style="left:' // trim(stamp_pos) // 'px">DONE ' // trim(date_text) // '</div>')
    end if

    call add('<form method="post" action="/delete" style="position:absolute;right:24px;bottom:-20px"><input type="hidden" name="h" value="' // id // &
             '"><details><summary>discard card</summary><button>yes, discard this card</button></details></form></div>')
  end subroutine

  subroutine render_page(fresh_id, fresh_day)
    integer, intent(in) :: fresh_id, fresh_day
    type(habit), allocatable :: h(:)
    type(punch), allocatable :: p(:)
    integer :: i, now

    call load_habits(h)
    call load_punches(p)
    now = today()

    call add('<!doctype html><html lang="en"><head><meta charset="utf-8"><title>Punch Card Habits</title>' // &
             '<meta name="viewport" content="width=1180"><style>' // CSS // '</style></head><body><main>' // &
             '<h1>PUNCH CARD HABITS</h1><p class="sub">Written in Fortran. One card per habit, one column per day.</p>')
    if (size(h) < MAX_HABITS) then
      call add('<form class="new" method="post" action="/add"><input name="name" maxlength="40" required autocomplete="off" ' // &
               'placeholder="NEW HABIT" aria-label="New habit"><button class="lever">KEY NEW CARD</button></form>')
    end if
    if (size(h) == 0) call add('<p class="empty">No cards yet. Key in your first habit above.</p>')
    ! Newest card on top, like a fresh card dropped onto the stack.
    do i = size(h), 1, -1
      call render_card(h(i), p, now, merge(fresh_day, -1, h(i)%id == fresh_id), h(i)%id == fresh_id .and. fresh_day == now)
    end do
    call add('<p class="help">Click any past column&#39;s digit to punch or fix that day. A new card starts every 80 days.</p></main></body></html>')
  end subroutine

  subroutine handle_add(body)
    character(len=*), intent(in) :: body
    type(habit), allocatable :: h(:)
    character(len=NAME_LEN) :: name
    integer :: next_id
    name = card_name(form_value(body, 'name'))
    if (len_trim(name) == 0) return
    call load_habits(h)
    if (size(h) >= MAX_HABITS) return
    next_id = 1
    if (size(h) > 0) next_id = maxval(h%id) + 1
    h = [h, habit(next_id, today(), name)]
    call save_habits(h)
  end subroutine

  subroutine handle_delete(body)
    character(len=*), intent(in) :: body
    type(habit), allocatable :: h(:)
    type(punch), allocatable :: p(:)
    integer :: id
    id = to_int(form_value(body, 'h'))
    call load_habits(h)
    call load_punches(p)
    h = pack(h, h%id /= id)
    p = pack(p, p%habit_id /= id)
    call save_habits(h)
    call save_punches(p)
  end subroutine

  ! Returns the query string that makes the new hole and the stamp animate.
  function handle_punch(body) result(location)
    character(len=*), intent(in) :: body
    character(len=:), allocatable :: location
    type(habit), allocatable :: h(:)
    integer :: id, day, i
    location = '/'
    id = to_int(form_value(body, 'h'))
    day = to_int(form_value(body, 'd'))
    call load_habits(h)
    do i = 1, size(h)
      if (h(i)%id == id) then
        if (day >= h(i)%created - CARD_COLUMNS .and. day <= today()) then
          if (toggle_punch(id, day)) location = '/?p=' // str(id) // '&d=' // str(day)
        end if
        return
      end if
    end do
  end function

  ! A form on some other website must not be able to post to this server.
  logical function foreign_origin(headers)
    character(len=*), intent(in) :: headers
    character(len=:), allocatable :: low, origin
    integer :: at, e, i
    allocate(character(len=len(headers)) :: low)
    do i = 1, len(headers)
      low(i:i) = lower(headers(i:i))
    end do
    foreign_origin = .false.
    at = index(low, CRLF // 'origin:')
    if (at == 0) return
    at = at + 9
    e = index(low(at:), CRLF) + at - 2
    origin = trim(adjustl(low(at:e)))
    foreign_origin = origin /= 'http://localhost:8080' .and. origin /= 'http://127.0.0.1:8080'
  end function

  subroutine handle(fd)
    integer(c_int), intent(in) :: fd
    character(len=8192) :: req
    character(len=:), allocatable :: line, method, path, query, low, body
    integer(c_size_t) :: n
    integer :: used, end_head, sp1, sp2, q, need, at, e, i, ios

    ! Keep reading until the headers are complete, then until the body is.
    used = 0
    end_head = 0
    need = 0
    do
      n = c_read(fd, req(used + 1:), int(len(req) - used, c_size_t))
      if (n <= 0 .or. n > len(req)) return
      used = used + int(n)
      if (end_head == 0) then
        end_head = index(req(:used), CRLF // CRLF)
        if (end_head > 0) then
          allocate(character(len=end_head) :: low)
          do i = 1, end_head
            low(i:i) = lower(req(i:i))
          end do
          at = index(low, CRLF // 'content-length:')
          if (at > 0) then
            at = at + 17
            e = index(low(at:) // CRLF, CRLF) + at - 2
            read(low(at:e), *, iostat=ios) need
            if (ios /= 0 .or. need < 0) need = 0
          end if
        end if
      end if
      if (end_head > 0 .and. used >= end_head + 3 + need) exit
      if (used == len(req)) then
        call respond(fd, '413 Content Too Large', 'text/plain', 'Too large' // char(10)); return
      end if
    end do

    line = req(:index(req(:used), CRLF) - 1)
    sp1 = index(line, ' ')
    sp2 = index(line, ' ', back=.true.)
    if (sp1 == 0 .or. sp2 <= sp1) then
      call respond(fd, '400 Bad Request', 'text/plain', 'Bad request' // char(10)); return
    end if
    method = line(:sp1 - 1)
    path = line(sp1 + 1:sp2 - 1)
    query = ''
    q = index(path, '?')
    if (q > 0) then
      query = path(q + 1:)
      path = path(:q - 1)
    end if
    body = req(end_head + 4:end_head + 3 + need)

    if (method == 'GET' .and. path == '/') then
      call render_page(to_int(form_value(query, 'p')), to_int(form_value(query, 'd')))
      call respond(fd, '200 OK', 'text/html; charset=utf-8', page_buf(1:page_len))
    else if (method == 'POST') then
      if (foreign_origin(req(:end_head + 1))) then
        call respond(fd, '403 Forbidden', 'text/plain', 'Forbidden' // char(10)); return
      end if
      select case (path)
      case ('/add')
        call handle_add(body)
        call respond(fd, '303 See Other', 'text/plain', '', 'Location: /')
      case ('/delete')
        call handle_delete(body)
        call respond(fd, '303 See Other', 'text/plain', '', 'Location: /')
      case ('/punch')
        call respond(fd, '303 See Other', 'text/plain', '', 'Location: ' // handle_punch(body))
      case default
        call respond(fd, '404 Not Found', 'text/plain', 'Not found' // char(10))
      end select
    else if (method == 'GET') then
      call respond(fd, '404 Not Found', 'text/plain', 'Not found' // char(10))
    else
      call respond(fd, '405 Method Not Allowed', 'text/plain', 'GET and POST only' // char(10), 'Allow: GET, POST')
    end if
  end subroutine
end module

program habits
  use sockets
  use web
  implicit none
  integer(c_int), parameter :: PORT = 8080
  integer(c_int) :: server, client, pid, ignored
  type(sockaddr_in) :: addr
  integer(c_intptr_t) :: old

  call execute_command_line('mkdir -p data')

  ! Let finished children vanish instead of staying as zombies.
  old = c_signal(SIGCHLD, 1_c_intptr_t)

  server = c_socket(AF_INET, SOCK_STREAM, 0)
  ignored = c_setsockopt(server, SOL_SOCKET, SO_REUSEADDR, 1, 4)

  addr%sin_len = 16
  addr%sin_family = AF_INET
  addr%sin_port = transfer(ior(ishft(iand(PORT, 255), 8), ishft(PORT, -8)), addr%sin_port)  ! network byte order
  addr%sin_addr = int(z'0100007F', c_int32_t)  ! 127.0.0.1, bytes reversed
  addr%sin_zero = 0

  if (c_bind(server, addr, 16) /= 0) stop 'Could not bind port 8080. Already running?'
  if (c_listen(server, 64) /= 0) stop 'listen failed'
  print '(a)', 'Punch Card Habits on http://localhost:8080'
  flush(6)

  do
    client = c_accept(server, c_null_ptr, c_null_ptr)
    if (client < 0) cycle
    ! One process per connection, so an idle browser connection can't block anyone.
    pid = c_fork()
    if (pid == 0) then
      ignored = c_close(server)
      ignored = c_alarm(30)
      call handle(client)
      ignored = c_close(client)
      stop
    end if
    ignored = c_close(client)
  end do
end program
