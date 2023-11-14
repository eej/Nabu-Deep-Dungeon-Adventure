
# Deep Dungeon Adventure for the Nabu.

![Deep Dungeon Adventure title screen](https://github.com/eej/Nabu-Deep-Dungeon-Adventure/raw/master/screenshots/title.png)
![Deep Dungeon Adventure dungeon screen shot](https://github.com/eej/Nabu-Deep-Dungeon-Adventure/raw/master/screenshots/dungeon1.png)

[Deep Dungeon Adventure](https://www.msx.org/downloads/deep-dungeon-adventure-source-code) is a Rougelike dungeon crawling game.  It was originally created for the MSX by ARTRAG, John Hassink and Huey of Trilobyte. Deep Dungeon Adventure won the [MSXdev08](https://www.msxdev.org/msxdev-archive/msxdev08/) competition.  

This port is based on the source code release of the enhanced version.  It has the full features of the original, including the easter egg.  It is available as a homebrew .nabu build and a CP/M .com build.

A word of warning regarding this source code: working on this port is how I learned Z80 assembly, Nabu architecture, and MSX architecture.  The code I wrote here is not particularly good and I'd recommend referring to the [MSX original](https://github.com/artrag/Deep-Dungeon-Adventure) instead.

# Enhancements

![Blood Drop Animated Gif](https://github.com/eej/Nabu-Deep-Dungeon-Adventure/raw/master/screenshots/blood.gif)

I made two small enhancements to the game.  One is flashing a blood drop icon to indicate when the player has taken damage.  The other is playing a very short sound effect when the player attacks a monster but doesn't kill it.

# Building

Building the project requires the [SJASMplus](https://github.com/z00m128/sjasmplus) assembler and the [MSX-o-Mizer](https://www.pouet.net/prod.php?which=50538) data compressor.

First build and compress the easter egg in the intruder4k_redux directory:

```
sjasmplus.exe --syntax=ab .\game2k.asm  --lst=game2k.lst --raw=game2k.bin
MSX-O-Mizer -r .\game2k.bin .\game2k.bin.miz
```

Then in the main directory, build the game:

```
sjasmplus main.asm --syntax=ab --lst=DeepDun.lst --raw=DeepDun.nabu
```

Use the `-DCPM` flag to build a CP/M .com file instead of the .nabu Homebrew file.


# Lessons Learned

The Nabu and MSX have very similar hardware making porting software relatively straightforward.  But they are different enough that it isn't trivial.  While there are too many differences to list here, I want to point out a few interesting gotchas that surprised me or tripped me up as a beginner.

## There is more than one way to call the BIOS

The MSX BIOS has a library of functions for interfacing with the hardware.  These routines aren't present on the Nabu, and you'll need to provide substitutions.  The main thing to watch out for is `call` instructions to BIOS address space.  But you also need to watch for `jp`, `jr`, and `rst` instructions used to enter BIOS routines.

## Register 7 on the AY-3

The MSX and Nabu both use the AY-3-8910 which, in addition to its sound capabilities, provides two general purpose I/O ports.  Bit's 7 and 6 of Register 7 of the AY-3 control whether each of the ports are configured for reading or writing.  Unfortunately these bits must be set to `01` on the Nabu and `10` on the MSX.  Bits 0 through 5 of register 7 are the mixer bits, used to enable or disable the tone and noise generators for each of the 3 audio channels.  Which means that audio playback code written for the MSX will likely need to be modified to ensure the high bits of register 7 are always set correctly for the Nabu.

## RAM challenges

Deep Dungeon Adventure was designed to run from a 48kb ROM cartridge on a MSX machine with at least 16kb of RAM.  On the Nabu there is no such thing as running a game from a ROM.  Happily, with 64kb, there is enough room to load the game in RAM and still have space for it to run.  But it needs almost all of the Nabu's memory.

CP/M uses some RAM to provide its BDOS routines for disk access and other functions.  The BDOS code is located at the top of the address space and takes 11~12kb on the Nabu.  Because the game doesn't need that functionality, it's okay to overwrite that memory.  But the stack pointer will be set below the start of BDOS and it needs to be moved if you want to use that space.

This isn't a concern with homebrew programs which start with the stack pointer set to $FFEE.  But the Nabu boot ROM loads programs starting at address $140D which creates a different challenge.  After loading a 48kb program, the free ram is split into a ~5kb chunk at the bottom of the address space and a ~11kb chunk at the top instead of a contiguous 16kb block.  I worked around this by adapting how the game uses RAM, but another option would have been to relocate the program to the start of the address space once it is loaded.

## MSX Interrupt Handler

Another feature of the of the MSX BIOS is that it handles the video interrupts. You can provide the BIOS a function pointer to it to run your own code at interrupt time, which is how Deep Dungeon Adventure does music updates.  The MSX interrupt handler saves and restores the _full_ CPU register state via the stack, freeing MSX programmers from having to worry about register use in the their interrupt functions.  I was frustrated by random seeming bugs in my port until I realized that the music updates were changing register states that weren't being managed by the interrupt handler I wrote for the Nabu.  C-BIOS, an open source MSX BIOS implementation, starts its interrupt handler with the following:
```
	push    hl
	push    de
	push    bc
	push    af
	exx
	ex      af,af'
	push    hl
	push    de
	push    bc
	push    af
	push    iy
	push    ix
```

## SJASMplus list syntax

This doesn't have anything to do with the Nabu or MSX, but instead is important if you are converting a codebase to assemble with SJASMPlus.  Use SJASM with the `--syntax=a` setting.

`sub 4` is a standard z80 instruction that subtracts 4 from register a.  `sub a, 4` is improper syntax, there is no version of the sub instruction that takes two parameters, register a is always implicit.  Many z80 assemblers accept this non-standard syntax as another way to write `sub 4`.  SJASM on the other hand has a "multi-argument" syntax feature where it interprets `sub a, 4` as a comma separated list of values that you want to subtract.  So in this example it produces two instructions: `sub a; sub 4`.  This is a bit of a trap generally, and particularly annoying when you're converting a codebase between assemblers.  The `--syntax=a` parameter tels SJASM to use `,,` as its list delimiter for the multi-argument feature making it harder to trigger by accident.

# Thanks

Thanks of course go out to the Trilobyte team for creating Deep Dungeon Adventure and sharing their source code.

This project also benefited greatly from the [NabuSoft Project](https://github.com/buricco/nabusoft/tree/main#the-nabusoft-project), which provided reference and useful code.  Thank you Licca for sharing your code and answering a lot of questions on Discord.
	