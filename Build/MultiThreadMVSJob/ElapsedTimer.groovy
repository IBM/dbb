import groovy.time.*

def now = new Date()
def now2 = new Date()

def start()
{
    now = now2 = new Date()
}

def TimeDuration pause()
{
    def duration = TimeCategory.minus(new Date(), now2)
    now2 = new Date()
    return duration
}

def TimeDuration stop()
{
    def duration = TimeCategory.minus(new Date(), now)
    now = new Date()
    return duration
}