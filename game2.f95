module subprograms
    implicit none
    character(1), dimension(:, :), allocatable :: board
    integer :: columns,rows,current_column,current_row,x,y,set_to_make,input,total_turn,computerIQ
    character :: tokken
    Logical :: game_status,player !player is reffered as first player
    contains
    !change_setting for game 
    recursive subroutine change_setting
        print *,"enter computer IQ :"
        print*,"press 0 for 2 player"
        read *,computerIQ
        if(computerIQ<0 .or. columns>7) then
            print *,"invalid"
            call change_setting
        end if 

        set_to_make=4
        print *,"you have to make set of  ",set_to_make

        print *,"for dimention change ,Enter 0"
        print *,"for default(7x6), enter 1"
        print *,"it should be greater than or equal to 4x4"
        read *,input

        if (input==0) then
            print *,"enter no of columns :"
            read *,columns
            print *,"enter no of rows :"
            read *,rows
            if(rows<set_to_make .or. columns<set_to_make) then
                print *,"invalid"
                call change_setting
            end if 
        else if(input==1) then
            columns=7
            rows=6
        else
            print *,"invalid"
            call change_setting
        end if
        end subroutine change_setting

    !initialize game before start
    subroutine initialize
        game_status=.TRUE.
        allocate(board(columns, rows));
        board = " "
        total_turn=0
        end subroutine initialize

    !display screen
    subroutine display
        do y = rows, 1, -1
            do x = 1, columns
                print '(3a1,$)', '[', board(x, y), ']'
            end do
        print *
        end do
        do x = 1, columns
            print "('[', I0, ']',$)", x
        end do
        print *," "
        print *,"------------------------------------------------------------------------------"
        end subroutine display


    !take input from user
    subroutine inputFromUser()
        REAL :: randomValue

        if(player) then
            tokken="*"
            print *,"its [*] turn, enter move (enter 0 of exit)"
            read *,input
            if (input==0) then
                print *,"exit game"
                game_status=.False.
            else if(input>columns .or. input<1) then
                print *,"invalid move"
            else
                check1 : do y=rows,1,-1
                if(board(input,rows)/=" ") then
                    print *,"columns is already filled"
                    exit check1
                else if(board(input,y)/=" ") then
                    board(input,y+1)=tokken
                    current_column=input
                    current_row=y+1
                    total_turn=total_turn+1
                    player = .NOT. player
                    exit check1
                else if(y==1) then
                    board(input,y)=tokken
                    current_column=input
                    current_row=y
                    total_turn=total_turn+1
                    player = .NOT. player
                    end if
                end do check1
                call display
                end if
        
        else if(computerIQ==0) then
            tokken="o"
            print *,"its [o] turn, enter move (enter -1 for undo) (enter 0 of exit)"
            read *,input
            if (input==0) then
                print *,"exit game"
                game_status=.False.
            else if(input>columns .or. input<1) then
                print *,"invalid move"
            else
                check2 : do y=rows,1,-1
                if(board(input,rows)/=" ") then
                    print *,"columns is already filled"
                    exit check2
                else if(board(input,y)/=" ") then
                    board(input,y+1)=tokken
                    current_column=input
                    current_row=y+1
                    total_turn=total_turn+1
                    player = .NOT. player
                    exit check2
                else if(y==1) then
                    board(input,y)=tokken
                    current_column=input
                    current_row=y
                    total_turn=total_turn+1
                    player = .NOT. player
                    end if
                end do check2
                call display
            end if

        else
            tokken="o"
            if(total_turn<4) then
                CALL RANDOM_NUMBER(randomValue)
                input = 1 + INT((columns - 2) * randomValue)
                check3 : do y=rows,1,-1
                if(board(input,rows)/=" ") then
                    print *,"columns is already filled"
                    exit check3
                else if(board(input,y)/=" ") then
                    board(input,y+1)=tokken
                    current_column=input
                    current_row=y+1
                    total_turn=total_turn+1
                    player = .NOT. player
                    exit check3
                else if(y==1) then
                    board(input,y)=tokken
                    current_column=input
                    current_row=y
                    total_turn=total_turn+1
                    player = .NOT. player
                    end if
                end do check3
                call display()
            else
                call computer()
            end if
        end if
        end subroutine inputFromUser

    !computer input
    subroutine computer()
        integer :: leftpremove
        integer, dimension(columns) :: points
        character(1), dimension(columns,rows) :: a1
        integer :: poin,y1
        integer :: move=4
        integer :: inp
        Logical :: temp
        points=0
        a1=board
        leftpremove=computerIQ
        do inp=1,columns
            !check4 loop check the top empty place in column and put input
            check4 : do y1=rows,1,-1
                poin=0
                if(a1(inp,rows)/=" ") then
                    points(inp)=-1*(columns**columns)
                    exit check4
                else if(board(inp,y1)/=" ") then
                    a1(inp,y1+1)="o"
                    call winning(a1,temp,inp,y1+1,.True.)
                    if(temp) then
                        poin=columns**leftpremove
                    else
                        call bestMove(poin,a1,.False.,leftpremove-1)
                    end if
                    points(inp)=poin
                    a1(inp,y1+1)=" "
                    exit check4
                else if(y1==1) then
                    a1(inp,y1)="o"
                    call winning(a1,temp,inp,y1,.True.)
                    if(temp) then
                        poin=columns**leftpremove
                    else
                        call bestMove(poin,a1,.False.,leftpremove-1)
                    end if
                    points(inp)=poin
                    a1(inp,y1)=" "
                end if
            end do check4
        end do

        do inp=1,columns
            ! print *,inp,points(inp)
            if(points(move)<points(inp)) then
                move=inp
            end if
        end do
        ! print *,"move is ",move
        inp=move
        check5 : do y1=rows,1,-1
            if(board(inp,rows)/=" ") then
                exit check5
            else if(board(inp,y1)/=" ") then
                board(inp,y1+1)=tokken
                current_column=inp
                current_row=y1+1
                total_turn=total_turn+1
                player = .NOT. player
                exit check5
            else if(y1==1) then
                board(inp,y1)=tokken
                current_column=inp
                current_row=y1
                total_turn=total_turn+1
                player = .NOT. player
                end if
        end do check5
        call display
    end subroutine computer


    recursive subroutine bestMove(points,a2,tok,leftpremove)
        integer, intent(in) :: leftpremove
        character(1), dimension(1:columns,1:rows), intent(in) :: a2
        character(1), dimension(1:columns,1:rows) :: a1
        integer, intent(inout) :: points
        Logical ,intent(in) ::tok
        Logical :: temp
        integer :: inp
        integer :: y1=0
        a1=a2
        if(leftpremove<=0) then
            !print *,"move selected ",points 
            return
        else if(tok) then
            do inp=1,columns
            check3 : do y1=rows,1,-1
                if(a1(inp,rows)/=" ") then
                    ! points=points-100
                    exit check3
                else if(a1(inp,y1)/=" ") then
                    a1(inp,y1+1)="o"
                    call winning(a1,temp,inp,y1+1,tok)
                    if(temp) then
                        points=points+columns**leftpremove
                    else
                        call bestMove(points,a1,.False.,leftpremove-1)
                    end if
                    a1(inp,y1+1)=" "
                    exit check3
                else if(y1==1) then
                    a1(inp,y1)="o"
                    call winning(a1,temp,inp,y1,tok)
                    if(temp) then
                        points=points+columns**leftpremove
                    else
                        call bestMove(points,a1,.False.,leftpremove-1)
                    end if
                    a1(inp,y1)=" "
                    exit check3
                end if
            end do check3
            end do
        else
            do inp=1,columns
                check4 :do y1=rows,1,-1
                    if(a1(inp,rows)/=" ") then
                        ! points=points-100
                        exit check4
                    else if(a1(inp,y1)/=" ") then
                        a1(inp,y1+1)="*"
                        call winning(a1,temp,inp,y1+1,tok)
                        if(temp) then
                            points=points-columns**leftpremove
                        else
                            call bestMove(points,a1,.True.,leftpremove-1)
                        end if
                        a1(inp,y1+1)=" "
                        exit check4
                    else if(y1==1) then
                        a1(inp,y1)="*"
                        call winning(a1,temp,inp,y1,tok)
                        if(temp) then
                            points=points-columns**leftpremove
                        else
                            call bestMove(points,a1,.True.,leftpremove-1)
                        end if
                        a1(inp,y1)=" "
                        exit check4
                    end if
                end do check4
            end do
        end if
    end subroutine bestMove

    subroutine winning(board,winn_check,column_,row_,tok)
        character(1), dimension(columns,rows), intent(in) :: board
        integer, intent(in) :: column_,row_
        Logical,intent(out) :: winn_check
        Logical, intent(in) :: tok
        character(1) :: tokken1
        integer :: count
        count=0
        winn_check = .False.

        if(tok) then
            tokken1="o"
        else
            tokken1="*"
        end if 

        !verticle
        do y=row_,1,-1
            if(board(column_,y)==tokken1) then
                count=count+1
                if(count>=set_to_make) then
                    winn_check=.TRUE.
                end if
            else
                count=0
            end if
        end do

        

        !horizontle
        count=0
        do x=1,columns
            if(board(x,row_)==tokken1) then
                count=count+1
                if(count>=set_to_make) then
                    winn_check=.TRUE.
                end if
            else
                count=0
            end if
        end do

        

        ! /slash
        count=0
        x=column_-set_to_make
        do y=row_-set_to_make,row_+set_to_make
            if((x>=1 .and. x<=columns) .and. (y>=1 .and. y<=rows)) then
                if(board(x,y)==tokken1) then
                    count=count+1
                    if(count>=set_to_make) then
                        winn_check=.TRUE.
                    end if
                else
                    count=0
                end if
            end if
            x=x+1
        end do 


        ! backslash
        count=0
        x=column_-set_to_make
        slash2 : do y=row_+set_to_make,row_-set_to_make,-1
            if((x>=1 .and. x<=columns) .and. (y>=1 .and. y<=rows)) then
                if(board(x,y)==tokken1) then
                    count=count+1
                    if(count>=set_to_make) then
                        winn_check=.TRUE.
                    end if
                else
                    count=0
                end if
            end if
            x=x+1
        end do slash2


        !draw check
        if(total_turn>=columns*rows) then
            print *,"game draw";
            game_status=.False.
        end if
    end subroutine winning
        
    end module subprograms

program show
    use subprograms
    integer :: t
    call change_setting
    call initialize
    call display
    player = .TRUE.
    kloop: do while(game_status)
        call inputFromUser()
        if(winn_check(current_column,current_row)) then
            print *,"Winner ",tokken
            exit kloop
        end if
        end do kloop


    contains
    Logical function winn_check(column_,row_)
        integer, intent(in) :: column_,row_
        Logical :: outp
        integer :: count
        count=0
        winn_check = .False.

        
        !verticle
        do y=row_,row_-set_to_make,-1
            if(board(column_,y)==tokken) then
                count=count+1
                if(count>=set_to_make) then
                    winn_check=.TRUE.
                end if
            else
                count=0
            end if
        end do

        

        !horizontle
        count=0
        do x=1,columns
            if(board(x,row_)==tokken) then
                count=count+1
                if(count>=set_to_make) then
                    winn_check=.TRUE.
                end if
            else
                count=0
            end if
        end do

        

        ! slash
        count=0
        x=column_-set_to_make
        do y=row_-set_to_make,row_+set_to_make
            if(board(x,y)==tokken) then
                count=count+1
                if(count>=set_to_make) then
                    winn_check=.TRUE.
                end if
            else
                count=0
            end if
            x=x+1
        end do


        ! backslash
        count=0
        x=column_-set_to_make
        do y=row_+set_to_make,row_-set_to_make,-1
            if(board(x,y)==tokken) then
                count=count+1
                if(count>=set_to_make) then
                    winn_check=.TRUE.
                end if
            else
                count=0
            end if
            x=x+1
        end do


        !draw check
        if(total_turn>=columns*rows) then
            print *,"game draw";
            game_status=.False.
        end if

        end function winn_check

end program show