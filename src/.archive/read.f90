program read
    real(8), DIMENSION(1) :: top
    real(8), DIMENSION(1,3) :: bot

    open(1, file='top.ip', status='unknown', form='unformatted')
    read(1) top
    close(1)

    open(1, file='bot.ip', status='unknown', form='unformatted')
    read(1) bot
    close(1)

    write(*,*) top
    write(*,*) bot
    
end program read
