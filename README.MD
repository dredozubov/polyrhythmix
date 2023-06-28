# Poly

Polyrhythmically-inclinded drum generator.

# Motivation

I'm a guitar player, and I use tablature notation editors such as Guitar Pro a lot. However, it gets complicated fast to write a polyrhythmic/polymetric drum parts as shifts tend to go over the bar lines. The other property of such parts is: it tend to unfold in a simple way from simple ideas such as "I want to create a drum part that will have a 3 against 4 feel with a kick drum against a snare drum". The other way to think about it is that it has a simple blueprint, but it's tricky to express in the western musical notation. This is why `Poly` exists. I wanted to have a simple tool to workshop/brainstorm rhythmic ideas and evaluate them by having a MIDI playback. I'm into modern progressive rock/metal music, fusion, so it all applies very well. I have an impression it may be useful for indian carnatic music as well, but I would like to get some insightful confirmation on that.

# Usage

Poly runs as an executable with the desired command line options. The available options are as follows:

```
Usage: poly [OPTIONS]

Options:
  -K, --kick <KICK>
          Kick drum pattern
  -S, --snare <SNARE>
          Snare drum pattern
  -H, --hi-hat <HIHAT>
          Hi-Hat pattern
  -C, --crash <CRASH>
          Crash cymbal pattern
  -t, --tempo <TEMPO>
          Tempo value [default: 120]
  -s, --time-signature <TIME_SIGNATURE>
          Time signature [default: 4/4]
  -o, --output-file <OUTPUT>
          Output file path, make a dry run if omitted
  -B, --follow-kick-drum-with-bass
          Generate a second MIDI track for the bass following the kick drum
  -h, --help
          Print help
  -V, --version
          Print version
```

Poly uses a simple DSL (Domain-specific language) for drum patterns. For a more detailed explanation, go to [DSL Overview](#dsl-overview).

Let's say you want to tell if two patterns will converge and how soon. We'll start with a 3 against 4. The first pattern would be a series of 8th notes on the kick drum

```
poly  --kick '8x--x--' --snare '4-x'
```
Output
```
No output file path was supplied, running a dry run...
Converges over 3 bars
```

We haven't provided an `--output-file` / `-o` parameter, so `poly` made a dry run. It tells us it will converge in 3 bars. Let's see how it will look in the MIDI file by adding an output.

```
poly  --kick '8x--x--' --snare '4-x' -o out.mid
```
Output
```
Converges over 3 bars
out.mid was written successfully
```

Poly operates under an assumption, that it's easy to replicate a pattern that converges over a finite number of bars in the DAW or tablature editor, so it only generates 3 bars of drums in this case. On Mac OS, I usually do something in lines of `poly <OPTIONS> -o out.mid && open out.mid` or `poly <OPTIONS> -o out.mid && open -a 'Guitar Pro 7' out.mid`.

This way it defaults to 4/4 as a time signature, but we may want to interpret this rhythmic pattern in 3/4 for example. Let's try it:

```
poly --time-signature '3/4' --kick '8x--x--' --snare '4-x' -o out.mid && open -a 'Guitar Pro 7' out.mid
```
Output:
```
Converges over 2 bars
out.mid was written successfully
```

Now we can see it converges in 2 bars, not 3. Honestly, I like the 4/4 host time signature better. Let's get back to it. Also, we can add a crash cymbal and hi-hat patterns too, also we can make it just a bit livelier by increasing the tempo:

```
poly --time-signature '4/4' --tempo 138 --crash '4x---' --hi-hat '8-xxx' --kick '8x--x--' --snare '4-x' -o out.mid
```

That's cool, but let's make it even more useful by adding a blueprint for the bass track. Simple way of doing that is to make bass follow the kick drum. `Poly` has an option to do this called `-B`/`--follow-kick-drum-with-bass`. Let's add it to the previous command to add the bass track to the output file:

```
poly --time-signature '4/4' --tempo 138 --crash '4x---' --hi-hat '8.t-xxx' --kick '8x--x--' --snare '4-x' -o out.mid -B
```

Now we have two tracks in the output file and you can change the bass notes to create an expected harmonic context.

# DSL overview

Any pattern can be described by a series of note groups. All note in the note group have the same length. Possible lengths are:
* `1` - Whole note
* `2` - Half note
* `4` - Fourth note
* `8` - Eighth note
* `16` - Sixteenth note
* `32` - Thirty secondth note
* `64` - Sixty Fourth note
* `.` - dotted note (meaning it has 1.5 length of the unmodified duration). Dot should be applied after the basic length like this: `8.`
* `t` - Triplet notes, should be applied after basic lengths and dots. e.g. `4.t` means triplets of dotted fourth notes.

Now let's talk about the drums. `Poly` has a logic similar to a drum machine, so we only concern ourselves with drum hits and rests:
* `x` - Hit
* `-` - Rest

Let's compose a few simple note groups:
* `4x` - a group of one fourth note.
* `8.-x` a group of a rest and a drum hit. Both rest and hit have a length of 8th dotted note each.

...

## Guitar pro remarks

Don't forget to quantize MIDI imports to 64th notes as it gets increasingly crazier as we get into the wilder note groupings:
![Guitar Pro Import](doc/Guitar-Pro-Import.png)



