! Turbo Wieszcz ++ Fortran (~95/GNU more/less) version, v1.0
! (c)2017-2019 Noniewicz.com, Jakub Noniewicz
!///////////////////////////////////////////////////////////////////////////////
!// based directly on (translated from): previous version written in Python
!// which was based directly on (translated from): previous version written for Windows in Delphi
!// which was based directly on: previous version written for Commodore C-64 sometime in 1993
!// by me (Jakub Noniewicz) and Freeak (Wojtek Kaczmarek)
!// which was based on:
!// idea presented in "Magazyn Amiga" magazine and implemented by Marek Pampuch.
!// also inspired by version written for iPhone by Tomek (Grych) Gryszkiewicz.
!// and versions written in C, JavaScript, Pascal, PHP and as CGI by Tomek (Grych) Gryszkiewicz.
!// note: there are also ZX Spectrum/ZX-81/Amstrad CPC (ASM Z80), Android (Java) versions, and more!
!///////////////////////////////////////////////////////////////////////////////
! created: 20171026 20:00-21:00
! updated: 20171026 21:30-22:40
! updated: 20171026 22:50-23:05
! updated: 20171027 00:00-01:05
! updated: 20171027 01:25-01:55
! updated: 20171027 18:15-19:10
! updated: 20171027 19:20-19:30
! updated: 20171027 19:50-19:55
! updated: 20171027 21:25-22:40
! updated: 20171028 15:05-15:30
! updated: 20171028 16:35-16:40
! updated: 20190925 13:20-13:22

! NOTES:
! 1. to install (some) Fortran on (some) Linux you may use:
! apt-get install gfortran
! 2. compiation command example:
! $ gfortran -std=gnu -O3 TurboWieszcz.f95 -o TurboWieszcz
! $ strip TurboWieszcz
! 3. It is unclear for me which (if any) standard of Fortran it is conforming to...
! I started with F95 but then it does not compile with -std=f95 anymore (only with -std=gnu)
! 4. optional (ordered) parametrs when calling executable are:
! [number of stanzas (1-32)] [verse mode (1-3 = ABAB, ABBA, AABB)] [repetitions ok (0, 1)]


module wieszcz_variables
  integer :: stanza_count, verse_mode, title_id
  logical :: repetitions_ok
  character(len=4), dimension(5):: ENDINGS1
  character(len=4), dimension(5):: ENDINGS2
  integer, dimension(3, 4) :: TRYB2ORDER
  character(len=32), dimension(33) :: title
  integer, dimension(4, (32)) :: stnumber
  integer, dimension(2, (32)) :: ending
  character(len=64), dimension(4, 32):: data
  character(len=1) :: lf
end module
 
program turbo_wieszcz
  use wieszcz_variables
  implicit none

  character(len=64*(32)*4+32+256) :: generate_poem
  character(len=8) :: arg
  integer :: tmp

! -- init data

  lf = char(10)
  stanza_count = 4
  verse_mode = 1
  title_id = -1
  repetitions_ok = .FALSE.

! ABAB, ABBA, AABB - mode
  TRYB2ORDER(1, 1) = 0+1
  TRYB2ORDER(1, 2) = 1+1
  TRYB2ORDER(1, 3) = 2+1
  TRYB2ORDER(1, 4) = 3+1
  TRYB2ORDER(2, 1) = 0+1
  TRYB2ORDER(2, 2) = 1+1
  TRYB2ORDER(2, 3) = 3+1
  TRYB2ORDER(2, 4) = 2+1
  TRYB2ORDER(3, 1) = 0+1
  TRYB2ORDER(3, 2) = 2+1
  TRYB2ORDER(3, 3) = 1+1
  TRYB2ORDER(3, 4) = 3+1

  ENDINGS1(1) = '.'
  ENDINGS1(2) = '...'
  ENDINGS1(3) = '.'
  ENDINGS1(4) = '!'
  ENDINGS1(5) = '.'

  ENDINGS2(1) = ''
  ENDINGS2(2) = '...'
  ENDINGS2(3) = ''
  ENDINGS2(4) = '!'
  ENDINGS2(5) = ''

  title(01) = 'Zagłada'
  title(02) = 'To już koniec'
  title(03) = 'Świat ginie'
  title(04) = 'Z wizytą w piekle'
  title(05) = 'Kataklizm'
  title(06) = 'Dzień z życia...'
  title(07) = 'Masakra'
  title(08) = 'Katastrofa'
  title(09) = 'Wszyscy zginiemy...'
  title(10) = 'Pokój?'
  title(11) = 'Koniec'
  title(12) = 'Koniec ludzkości'
  title(13) = 'Telefon do Boga'
  title(14) = 'Wieczne ciemności'
  title(15) = 'Mrok'
  title(16) = 'Mrok w środku dnia'
  title(17) = 'Ciemność'
  title(18) = 'Piorunem w łeb'
  title(19) = 'Marsz troli'
  title(20) = 'Szyderstwa Złego'
  title(21) = 'Okrponości świata'
  title(22) = 'Umrzeć po raz ostatni'
  title(23) = 'Potępienie'
  title(24) = 'Ból mózgu'
  title(25) = 'Wieczne wymioty'
  title(26) = 'Zatrute dusze'
  title(27) = 'Uciekaj'
  title(28) = 'Apokalipsa'
  title(29) = 'Złudzenie pryska'
  title(30) = 'Makabra'
  title(31) = 'Zagłada świata'
  title(32) = 'Śmierć'
  title(33) = 'Spokój'

!///////////////////////////////////////////////
!//po 10
  data(1, 1)  = 'Czy na te zbrodnie nie będzie kary?'
  data(1, 2)  = 'Opustoszały bagna, moczary'
  data(1, 3)  = 'Na nic się modły zdadzą ni czary'
  data(1, 4)  = 'Z krwi mordowanych sączą puchary'
  data(1, 5)  = 'To nietoperze, węże, kalmary'
  data(1, 6)  = 'Próżno nieszczęśni sypią talary'
  data(1, 7)  = 'Za co nam znosić takie ciężary'
  data(1, 8)  = 'Złowrogo iskrzą kóbr okulary'
  data(1, 9)  = 'Próżno swe modły wznosi wikary'
  data(1, 10) = 'Pustoszą sny twoje złe nocne mary'
  data(1, 11) = 'Próżno nieszczęśnik sypie talary'
  data(1, 12) = 'Przedziwnie tka się życia logarytm'
  data(1, 13) = 'Już Strach wypuścił swoje ogary'
  data(1, 14) = 'Niebawem zginiesz w szponach poczwary'
  data(1, 15) = 'Wbijają pale złote kafary'
  data(1, 16) = 'Życie odkrywa swoje przywary'
  data(1, 17) = 'Na dnie ponurej, pustej pieczary'
  data(1, 18) = 'Apokalipsy nadeszły czary'
  data(1, 19) = 'Upadły anioł wspomina chwałę'
  data(1, 20) = 'Życie ukrywa swoje przywary'
  data(1, 21) = 'Dziwnych owadów wzlatują chmary'
  data(1, 22) = 'Bombowce biorą nasze namiary'
  data(1, 23) = 'Nie da się chwycić z czartem za bary'
  data(1, 24) = 'Próżno frajerzy sypią talary'
  data(1, 25) = 'Nie da sie wyrwać czartom towaru'
  data(1, 26) = 'Po co nam sączyć podłe browary'
  data(1, 27) = 'Diler już nie dostarczy towaru'
  data(1, 28) = 'Lokomotywa nie ma już pary'
  data(1, 29) = 'Gdy nie każdego stać na browary'
  data(1, 30) = 'Pożarł Hilary swe okulary'
  data(1, 31) = 'Spowiły nas trujące opary'
  data(1, 32) = 'To nie jest calka ani logarytm'
!///////////////////////////////////////////////
!//po 8
  data(2, 1)  = 'Już na arenę krew tryska'
  data(2, 2)  = 'Już piana cieknie im z pyska'
  data(2, 3)  = 'Już hen w oddali gdzieś błyska'
  data(2, 4)  = 'Śmierć w kącie czai się bliska'
  data(2, 5)  = 'Niesamowite duchów igrzyska'
  data(2, 6)  = 'Już zaciskając łapiska'
  data(2, 7)  = 'Zamiast pozostać w zamczyskach'
  data(2, 8)  = 'Rzeka wylewa z łożyska'
  data(2, 9)  = 'Nieszczęść wylała się miska'
  data(2, 10) = 'Już zaciskając zębiska'
  data(2, 11) = 'Otwarta nieszczęść walizka'
  data(2, 12) = 'Niczym na rzymskich boiskach'
  data(2, 13) = 'Czart wznieca swe paleniska'
  data(2, 14) = 'A w mroku świecą zębiska'
  data(2, 15) = 'Zewsząd dochodzą wyzwiska'
  data(2, 16) = 'Świętych głód wiary przyciska'
  data(2, 17) = 'Ponuro patrzy z ich pyska'
  data(2, 18) = 'Mgła stoi na uroczyskach'
  data(2, 19) = 'Kości pogrzebią urwiska'
  data(2, 20) = 'Głód wiary tak nas przyciska'
  data(2, 21) = 'Runęły skalne zwaliska'
  data(2, 22) = 'Czart rozpala paleniska'
  data(2, 23) = 'A w mroku słychać wyzwiska'
  data(2, 24) = 'Znów pusta żebraka miska'
  data(2, 25) = 'Diabelskie to są igrzyska'
  data(2, 26) = 'Nie powiedz diabłu nazwiska'
  data(2, 27) = 'Najgłośniej słychać wyzwiska'
  data(2, 28) = 'Diabelskie mają nazwiska'
  data(2, 29) = 'Tam uciekają ludziska'
  data(2, 30) = 'Tak rzecze stara hipiska'
  data(2, 31) = 'Gdzie dawne ludzi siedliska'
  data(2, 32) = 'Najgłośniej piszczy hipiska'
!///////////////////////////////////////////////
!//po 10
  data(3, 1)  = 'Rwą pazurami swoje ofiary'
  data(3, 2)  = 'Nic nie pomoże tu druid stary'
  data(3, 3)  = 'To nocne zjawy i senne mary'
  data(3, 4)  = 'Niegroźne przy nich lwowskie batiary'
  data(3, 5)  = 'Pod wodzą księżnej diablic Tamary'
  data(3, 6)  = 'Z dala straszliwe trąbia fanfary'
  data(3, 7)  = 'Skąd ich przywiodły piekła bezmiary'
  data(3, 8)  = 'Zaś dookoła łuny, pożary'
  data(3, 9)  = 'A twoje ciało rozszarpie Wilk Szary'
  data(3, 10) = 'Tu nie pomoże już siła wiary'
  data(3, 11) = 'Tak cudzych nieszczęść piją nektary'
  data(3, 12) = 'Wszystko zalewa wrzący liparyt'
  data(3, 13) = 'Zabójcze są ich niecne zamiary'
  data(3, 14) = 'Zatrute dusze łączą się w pary'
  data(3, 15) = 'Świat pokazuje swoje wymiary'
  data(3, 16) = 'Z życiem się teraz weźmiesz za bary'
  data(3, 17) = 'Brak uczuć, chęci, czasem brak wiary'
  data(3, 18) = 'Wspomnij, co mówił Mickiewicz stary'
  data(3, 19) = 'Spalonych lasów straszą hektary'
  data(3, 20) = 'Z życiem się dzisiaj weźmiesz za bary'
  data(3, 21) = 'Ksiądz pozostaje nagle bez wiary'
  data(3, 22) = 'Papież zaczyna odprawiać czary'
  data(3, 23) = 'Tu nie pomoże paciorek, stary'
  data(3, 24) = 'Niegroźne przy nich nawet Atari'
  data(3, 25) = 'Takie są oto piekła bezmiary'
  data(3, 26) = 'A teraz nagle jesteś już stary'
  data(3, 27) = 'Mordercy liczą swoje ofiary'
  data(3, 28) = 'I bez wartości są już dolary'
  data(3, 29) = 'Gdzie się podziały te nenufary'
  data(3, 30) = 'Upada oto dąb ten prastary'
  data(3, 31) = 'Bystro śmigają nawet niezdary'
  data(3, 32) = 'Już nieruchome ich awatary'
!///////////////////////////////////////////////
!//po 8
  data(4, 1)  = 'Wnet na nas też przyjdzie kryska'
  data(4, 2)  = 'Znikąd żadnego schroniska'
  data(4, 3)  = 'Powietrze tnie świst biczyska'
  data(4, 4)  = 'Rodem z czarciego urwiska'
  data(4, 5)  = 'I swąd nieznośny się wciska'
  data(4, 6)  = 'Huk, jak z wielkiego lotniska'
  data(4, 7)  = 'Złowroga brzmią ich nazwiska'
  data(4, 8)  = 'W kącie nieśmiało ktoś piska'
  data(4, 9)  = 'Ktoś obok morduje liska'
  data(4, 10) = 'Krwią ociekają zębiska'
  data(4, 11) = 'Wokoło dzikie piarżyska'
  data(4, 12) = 'I żądza czai się niska'
  data(4, 13) = 'Diabeł cię dzisiaj wyzyska'
  data(4, 14) = 'Płoną zagłady ogniska'
  data(4, 15) = 'Gwałt niech się gwałtem odciska!'
  data(4, 16) = 'Stoisz na skraju urwiska'
  data(4, 17) = 'Tam szatan czarta wyiska'
  data(4, 18) = 'Uciekaj, przyszłość jest mglista'
  data(4, 19) = 'Nadziei złudzenie pryska'
  data(4, 20) = 'Wydziobią oczy ptaszyska'
  data(4, 21) = 'Padają łby na klepisko'
  data(4, 22) = 'Śmierć zbiera żniwo w kołyskach'
  data(4, 23) = 'Coś znowu zgrzyta w łożyskach'
  data(4, 24) = 'Spadasz z wielkiego urwiska'
  data(4, 25) = 'Lawa spod ziemi wytryska'
  data(4, 26) = 'Wokoło grzmi albo błyska'
  data(4, 27) = 'Fałszywe złoto połyska'
  data(4, 28) = 'Najwięcej czart tu uzyska'
  data(4, 29) = 'Owieczki Zły tu pozyska'
  data(4, 30) = 'Owieczki spadły z urwiska'
  data(4, 31) = 'Snują się dymy z ogniska'
  data(4, 32) = 'To czarne lecą ptaszyska'

! -- get args if any

  call getarg (1, arg)
  if (len(trim(arg)) > 0) then
    read(arg(1:2), '(i2)') stanza_count
    if ((stanza_count < 1) .or. (stanza_count > 32)) then
      stanza_count = 4  ! fallback to default 4 if out of range
    end if
  end if
  call getarg (2, arg)
  if (len(trim(arg)) > 0) then
    read(arg(1:1), '(i1)') verse_mode
    if ((verse_mode < 1) .or. (verse_mode > 3)) then
      verse_mode = 1
    end if
  end if
  call getarg (3, arg)
  if (len(trim(arg)) > 0) then
    read(arg(1:1), '(i1)') tmp
    if (tmp == 0) then
      repetitions_ok = .FALSE.
    else
      repetitions_ok = .TRUE.
    end if
  end if

! -- generate poem

  call init_random_seed()
  print *, trim(generate_poem())

end program turbo_wieszcz


! -- functions

!this one rather magic thing is from here
!https://stackoverflow.com/questions/31174367/slow-random-seed-generator-why
subroutine init_random_seed()
  integer :: i, n, clock
  integer, dimension(:), allocatable :: seed
  call random_seed(size = n)
  allocate(seed(n))
  call system_clock(count=clock)
  seed = clock + 37 * (/ (i - 1, i = 1, n) /)
  call random_seed(put = seed)
  deallocate(seed)
end

function check_uniq_ok(z, w, va) result(ok)
  use wieszcz_variables
  integer, intent(in) :: z, w, va
  integer :: i
  logical :: ok

  ok = .TRUE.
  if (.not. repetitions_ok) then
    do i=1,z-1
      if (stnumber(w, i) == va) then
        ok = .FALSE.
      end if
    end do
  end if
end function check_uniq_ok

subroutine set_random_row(z, w)
  use wieszcz_variables
  integer, intent(in) :: z, w
  real :: r
  logical :: check_uniq_ok

  do
    call RANDOM_NUMBER(r)
    stnumber(w, z) = 1+int(r*size(data(w, :)))
    if (z == 1) then
      exit
    end if
    if (check_uniq_ok(z, w, stnumber(w, z))) then
      exit
    end if
  end do
end subroutine set_random_row

function build_ending(z, w, w0) result(s)
  use wieszcz_variables
  integer, intent(in) :: z, w
  integer :: n
  character(len=64) :: w0
  character(len=8) :: s
  logical :: chk

  chk = .TRUE.
  s = ''
  n = len(trim(w0))
  if (n > 0) then
    if ((w0(n:n) == '?') .or. (w0(n:n) == '!')) then
      chk = .FALSE.
    end if
  end if
  if ((w == 1) .and. chk) then
    s = ENDINGS2(ending(1, z))
  end if
  if ((w == 3) .and. chk) then
    s = ENDINGS1(ending(2, z))
  end if
end function build_ending

function build_line(z, w, w0) result(s)
  use wieszcz_variables
  integer, intent(in) :: z, w, w0
  character(len=64) :: s
  character(len=8) build_ending

  s = trim(data(w, stnumber(w, z))) // trim(build_ending(z, w0, s)) // lf
end function build_line

function build_stanza(z) result(s)
  use wieszcz_variables
  integer, intent(in) :: z
  character(len=64*4) :: s
  character(len=64) build_line

  s = trim(build_line(z, TRYB2ORDER(verse_mode, 1), 1))
  s = trim(s) // trim(build_line(z, TRYB2ORDER(verse_mode, 2), 2))
  s = trim(s) // trim(build_line(z, TRYB2ORDER(verse_mode, 3), 3))
  s = trim(s) // trim(build_line(z, TRYB2ORDER(verse_mode, 4), 4))
end function build_stanza

function generate_poem() result(s)
  use wieszcz_variables
  character(len=64*(32)*4+32+256) :: s
  real :: r
  integer :: w, z, x
  character(len=64*4) :: build_stanza
 
  call RANDOM_NUMBER(r)
  title_id = 1+int(r*size(title))
  do z=1,stanza_count
    do w=1,4
      stnumber(w, z) = -1
    end do
    call RANDOM_NUMBER(r)
    ending(1, z) = 1+int(r*len(ENDINGS2))
    call RANDOM_NUMBER(r)
    ending(2, z) = 1+int(r*len(ENDINGS1))
    call set_random_row(z, 1)
    call set_random_row(z, 2)
    call set_random_row(z, 3)
    call set_random_row(z, 4)
  end do

  s = lf // trim(title(title_id)) // lf // lf
  do z=1,stanza_count
    s = trim(s) // trim(build_stanza(z)) // lf
  end do
end function generate_poem

subroutine dump_data()
  use wieszcz_variables
  integer :: w, z
  character(len=4) :: z1, w1

  do z=1,32
    do w=1,4
      write (z1, "(I2)") z
      write (w1, "(I2)") w
      print *, z1 // w1 // trim(data(w, z))
    end do
  end do
end subroutine dump_data
 