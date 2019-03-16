HELLO    CSECT
         USING *,15
         STM   14,12,12(13)
         LR    12,15
         USING HELLO,12
         WTO   'HI'
         XR    15,15
         RETURN (14,12)
         END
