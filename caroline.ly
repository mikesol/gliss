\version "2.15.40"

#(set-global-staff-size 15)

\paper {
  indent = #0
  ragged-last-bottom = ##f
  paper-width = 190\mm
  paper-height = 270
  footnote-separator-markup = \markup \null
}

tagForPiano = #(define-music-function (parser location piano? m)
  (boolean? ly:music?)
#{
  \keepWithTag #(if piano? 'piano 'voice)
  \removeWithTag #(if piano? 'voice 'piano)
  $m
#})

smallNotes = #(define-music-function (parser location n) (number?)
#{
  \stemUp
  \set fontSize = #n
  \override Stem #'length-fraction = #(magstep n)
  \override Beam #'length-fraction = #(magstep n)
  \override Beam #'beam-thickness = #0.35
#})

italianNotes = \smallNotes #-4
footnoteNotes = {
  \smallNotes #-1
  \override Score . StaffSymbol #'transparent = ##t
  \override Staff . Clef #'stencil = ##f
  \override Staff . TimeSignature #'stencil = ##f
}

footnoteOne = \markup {
  \general-align #Y #DOWN {
    \line {
      \concat { "*)" \transparent * }
      Suggested performance / Ausführungsvorschlag:
    }
    \score {
      \new Staff {
        \autoBeamOff
        \footnoteNotes
        g'8. g'16
      }
      \layout {}
    }
  }
}

footnoteTwo = \markup {
  \general-align #Y #DOWN {
    \line {
      "**)" Suggested performance / Ausführungsvorschlag:
    }
    \score {
      \new Staff {
        \footnoteNotes
        g'8. g'16
      }
      \layout {}
    }
  }
}

sopranoA = \relative c'' {
  \tag #'voice { r4 }
  \tag #'piano { bes4 }
  bes8. c16 d4 d8. e16 |
  f4 f,8 f c'4 c8. bes16 |
  a4 a8. g16 f4. f8 |
  bes4 bes8. a16 g4 a8. bes16 |
}

sopranoB = \relative c'' {
  c4 d8. e16 f2 ~ |
  f4
  \tag #'voice {
    bes,8. c16 d4
  }
  \tag #'piano {
    <bes d>8. c16 <bes d>4
  }
}

sopranoAltoSharedC = \relative c'' {
  g4 ees8. f16 g4 aes8. g16 |
  f4 d8. ees16 f4 g8.[ a16] |
}

sopranoC = \relative c'' {
  \tag #'voice {
    ees8. f16 |
    \stemDown g2 r2 |
    R1 |
  }
  \tag #'piano {
    ees8. f16 |
    \transpose c c' \sopranoAltoSharedC
  }
}

sopranoItalianNotes = {
  \italianNotes
  s1*6 |
  s32 g''4*7/8 g''4 s2 |
  s1 |
}

sopranoEnglish = \lyricmode {
  if there was a -- ny praise,
  if there was a -- ny vir -- tue,
  and if there was a -- ny praise,
  if there was a -- ny praise,
  if there was a -- ny praise,
}

sopranoItalian = \lyricmode {
  \override LyricText #'font-shape = #'italic
  bel -- la pu -- ra vir -- tu -- te
  fu so -- lo~il suo fi -- ne,
  fu sol, fu
  so -- lo~il suo fi -- ne,
  fu so -- lo~il suo fi --
  ne, fu so -- lo~il
  \set associatedVoice = "sopranoitalian"
  suo fi -- ne.
}

altoA = \relative c'' {
  \tag #'voice {
    \stemDown bes2 \stemNeutral r2 |
    r2 r4
  }
  \tag #'piano {
   <f bes>2( bes) |
   a s4 % Erreur dans Bärenreiter...
  }
  c,8 c |
  f4 f8. e16
  \tag #'voice { d4 d8. d16 | g4. f8 }
  \tag #'piano { d4. d8 | g4 g8. f16 }
  e4 e8. d16 |
}

altoB = \relative c' {
  c2 \partcombineApartOnce r4 bes8. c16 |
  d4 d8. ees16
  f4
}

altoTenorSharedC = \relative c' {
  g8. 
  \once \override TextScript #'X-extent = #'(-3 . 3)
  \tag #'voice { \footnote \markup "*" #'(0 . 0) \footnoteTwo aes16^"**)" }
  \tag #'piano { aes16 }
  bes4 bes8. c16 |
  d4 bes8. c16 d4
}


altoC = \relative c'' {
  \tag #'voice {
    bes8 \footnote \markup "*" #'(0 . 0) \footnoteOne aes8^"*)"|
    \sopranoAltoSharedC
  }
  \tag #'piano {
    \voiceTwo
    bes4 ~ |
    bes \transpose c c' \tagForPiano ##t \altoTenorSharedC
    c4 |
  }
}

altoItalianNotes = {
  \italianNotes
  s32 bes'4*7/8 bes'4 s2 |
  s1*7 |
}

altoEnglish = \lyricmode {
  praise,
  if there was a -- ny vir -- tue,
  and if there was a -- ny praise,
  if there was,
  if there was a -- ny praise,
  if there was a -- ny vir -- tue,
  and if there
}

altoItalian = \lyricmode {
  \override LyricText #'font-shape = #'italic
  fi -- \set associatedVoice = "alto"
  ne,
  bel -- la pu -- ra vir -- tu -- te
  che ha per pre -- mio la lo -- de,
  bel -- la pu -- ra vir -- tu -- te
  fu so -- lo~il suo fi -- ne,
  fu
}

tenorA = \relative c'' {
  g4 \tag #'voice \stemDown d2 \tag #'voice \stemNeutral c8 bes |
  \tag #'voice {
    aes[ g] f4
  }
  \tag #'piano {
    c'2
  }
  r2 |
  r4 f,8 f8 bes4 bes8. a16 |
  g4 a8 bes c4. bes8 |
}

tenorB = \relative c' {
  a4 a8. g16 f4 g8. a16 |
  bes2 ~ bes4
}

tenorC = \relative c' {
  c8. d16 |
  \tag #'piano { \stemUp \voiceOne }
  ees4
  \tag #'voice {
    \tagForPiano ##f \altoTenorSharedC
    f |
  }
  \tag #'piano {
    bes,4 ees2 |
    d4 f8. ees16 d4 f |
  }
}

tenorItalianNotes = {
  \italianNotes
  s4 s32 d'4*7/8 d'4 s4 |
  s1*7 |
}

tenorEnglish = \lyricmode {
  vir -- tue, a -- ny vir -- tue
  if there was a -- ny vir -- tue,
  and if there was a -- ny praise,
  if there was a -- ny praise,
  if there was a -- ny vir -- tue,
  and if there
}

tenorItalian = \lyricmode {
  \override LyricText #'font-shape = #'italic
  \set associatedVoice = "tenoritalian"
  tu -- te,
  \set associatedVoice = "tenor"
  pu -- ra vir -- tu -- te,
  bel -- la pu -- ra vir -- tu -- te
  che ha per pre -- mio, per pre -- mio la lo -- de,
  la lo -- de fu so -- lo~il suo fi -- ne,
  fu so -- lo,
}
bassA = \relative c' {
  g2. g4 |
  \tag #'voice { f2 r2 | R1*2 | }
  \tag #'piano { f2. r4 | r4 s1*7/4 | }
}

bassB = {
  \tag #'voice { R1*2 }
  \tag #'piano { s1*7/4 }
}

bassC = \relative c {
  \tag #'piano { s4 | }
  ees1 |
  bes'2. a4 |
}

bassItalianNotes = {
  \italianNotes
  s32 g1*31/32 |
  \hideNotes f2 \unHideNotes s2 |
  s1*5 |
  s2. s32 a8*3/4 a8 |
}

bassEnglish = \lyricmode {
  on those things,
  she thought __ _
}

bassItalian = \lyricmode {
  \override LyricText #'font-shape = #'italic
  fi -- \set associatedVoice = "bass" ne
  fu \set associatedVoice = "bassitalian"
  so -- lo~il suo
}

ignoreCollisionOnce = \once \override NoteColumn #'ignore-collision = ##t

pianoRH = <<
{
  \partcombine
    { \tagForPiano ##t \sopranoA }
    { \tagForPiano ##t \altoA }
  \tagForPiano ##t \sopranoB
  \partcombine
    { \tagForPiano ##t \sopranoC }
    { \tagForPiano ##t \altoC }
}
\new Voice = "changes" {
    s1 | s1 | s1 |
    s2...
    \showStaffSwitch
    \hideNotes \ignoreCollisionOnce d'16 |
    \change Staff = down \ignoreCollisionOnce c'2
    s2 |
    s2 \ignoreCollisionOnce f'4 \change Staff = up \ignoreCollisionOnce bes'4 |
    s1 | s1 |
    \unHideNotes
}
>>

pianoLH = {
  \partcombine
    { \tagForPiano ##t \tenorA }
    { \tagForPiano ##t \bassA }
  \partcombine
    { \tagForPiano ##t \altoB }
    { \tagForPiano ##t \tenorB }
  \partcombine
    { \tagForPiano ##t \tenorC }
    { \tagForPiano ##t \bassC }
}

globals = {
  \set Score.currentBarNumber = #118
  \bar ""
  \once \override Score . TimeSignature #'stencil = ##f
  \key bes \major
}

cGlobals = {
  \globals
  \autoBeamOff
}

pGlobals = {
  \globals
}

sGlobals = \cGlobals
aGlobals = \cGlobals
tGlobals = { \clef "treble_8" \cGlobals }
bGlobals = { \clef "bass" \cGlobals }
pRHGlobals = \pGlobals
pLHGlobals = { \clef bass \pGlobals }

#(define (define-grob-property symbol type? description)
  (if (not (equal? (object-property symbol 'backend-doc) #f))
      (ly:error (_ "symbol ~S redefined") symbol))

  (set-object-property! symbol 'backend-type? type?)
  (set-object-property! symbol 'backend-doc description)
  symbol)

#(define-grob-property 'link-me boolean? "Link two grobs.")

\score {
  <<
    \new ChoirStaff <<
      \new Staff = "sopranofull" <<
        <<
          \new Voice = "soprano" {
            \sGlobals \tagForPiano ##f { \stemNeutral \sopranoA \sopranoB \sopranoC }
          }
        \\
          \new Voice = "sopranoitalian" {
            \sopranoItalianNotes
          }
        >>
        \new Lyrics \with { alignBelowContext = sopranofull }
          \lyricsto "soprano" \sopranoItalian
        \new Lyrics \with { alignBelowContext = sopranofull }
          \lyricsto "soprano" \sopranoEnglish
       >>
      \new Staff = "altofull" <<
        <<
          \new Voice = "alto" {
            \aGlobals \tagForPiano ##f { \stemNeutral \altoA \altoB \altoC }
          }
        \\
          \new Voice = "altoitalian" {
            \altoItalianNotes
          }
        >>
        \new Lyrics \with { alignBelowContext = altofull }
          \lyricsto "altoitalian" \altoItalian
        \new Lyrics \with { alignBelowContext = altofull }
          \lyricsto "alto" \altoEnglish
       >>
      \new Staff = "tenorfull" <<
        <<
          \new Voice = "tenor" {
            \tGlobals \tagForPiano ##f { \stemNeutral \tenorA \tenorB \tenorC }
          }
        \\
          \new Voice = "tenoritalian" {
            \tenorItalianNotes
          }
        >>
        \new Lyrics \with { alignBelowContext = tenorfull }
          \lyricsto "tenor" \tenorItalian
        \new Lyrics \with { alignBelowContext = tenorfull }
          \lyricsto "tenor" \tenorEnglish
       >>
      \new Staff = "bassfull" <<
        <<
          \new Voice = "bass" {
            \bGlobals \tagForPiano ##f { \stemNeutral \bassA \bassB \bassC }
          }
        \\
          \new Voice = "bassitalian" {
            \autoBeamOff \bassItalianNotes
          }
        >>
        \new Lyrics \with { alignBelowContext = bassfull }
          \lyricsto "bassitalian" \bassItalian
        \new Lyrics \with { alignBelowContext = bassfull }
          \lyricsto "bass" \bassEnglish
       >>
    >>
    \new PianoStaff <<
      \new Staff = up \with { printPartCombineTexts = ##f }
        { \pRHGlobals \pianoRH }
      \new Staff = down \with { printPartCombineTexts = ##f }
        { \pLHGlobals \pianoLH }
    >>
  >>
  \layout {
    \context {
      \Lyrics
      fontSize = #-0.5
    }
    \context {
      \Score
      \override VoiceFollower #'after-line-breaking = ##f
      \override VoiceFollower #'style = #'dashed-line
    }
  }
}
