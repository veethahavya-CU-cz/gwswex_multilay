program WRITE
    real(8), DIMENSION(1) :: top
    real(8), DIMENSION(3,1) :: bot

    top(1) = 150
    bot(1,1) = top(1) - 5
    bot(2,1) = top(1) - 15
    bot(3,1) = top(1) - 30

    open(1, file='top.ip', status='unknown', form='unformatted')
    write(1) top
    close(1)

    open(1, file='bot.ip', status='unknown', form='unformatted')
    write(1) bot
    close(1)
    
end program write