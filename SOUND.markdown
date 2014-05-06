Holy cow, getting sound working in Haskell has been an ENORMOUS pain. There is no library with a simple function `playSound` where you give the path to a sound file and it plays the sound.

First I tried this: http://stackoverflow.com/questions/14005592/play-a-wav-file-with-haskell

And I couldn't get the libraries installed. So I scoured the net and tried a bunch of other solutions, couldn't get anything working. Got back to that link and got it working (I had to install SDL and SDL_mixer through brew first). And THEN, those packages aren't configured for Haskell correctly, so I had to change the config manually using `ghc-pkg describe SDL > sdl.pkg`, update the file, and `ghc-pkg update --global sdl.pkg`. I added a basics entry on how to do this, check it out.

THEN it turns out, I have to build with `-threaded`, and that wasn't working.
Then that randomly started working with no change on my part, but it wasn't showing the window and playing sound in parallel!
THEN I found out, SDL-mixer uses some unsafe functions, so they block all threads. I found a fix here: http://stackoverflow.com/questions/18155302/using-gloss-to-run-a-simulation-while-using-sdl-to-play-a-sound

THEN it turns out that even though both are working together, something is eating up my CPU and I couldn't even move the character!!
Then I decided to use a third-party command line app. I just used backticks to `mpg123` and played mp3 files through that.
So now I could do both audio and graphics in parallel, and move the guy around. And I could even play multiple audio files at the same time, which was awesome.
But now, when I close the window, the sound still keeps playing! I don't know why...when the main thread dies, all forked threads are supposed to die. But not in this case. Still can't figure it out.
So in summary, audio is a giant pain.

...And we're back. I couldn't figure out the issue with sounds, and then Leaf pointed out that this library is now totally UN-portable...I require all my users to have mpg123 (which I already knew). So I tried going back to SDL. I added a threaddelay before I do anything else, and now it somehow magically works. WTF.

And now I've started getting this error:

    actionkid(7549,0x10e5cb000) malloc: *** error for object 0x7fa808419570: pointer being freed was not allocated
    *** set a breakpoint in malloc_error_break to debug
    actionkid(7549,0x105004000) malloc: *** error for object 0x7fa808419570: pointer being freed was not allocated
    *** set a breakpoint in malloc_error_break to debug
    Abort trap: 6

...this is bullshit.
