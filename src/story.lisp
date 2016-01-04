;; Alluring Allegory - A story driven game
;; Copyright (C) 2015 Matthew Carter
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;; story.lisp

(in-package :cl-user)
(defpackage alluring-allegory.story
  (:use :cl
        :alluring-allegory.base
        :alluring-allegory.scene
        :alluring-allegory.choice
        :alluring-allegory.actor
        :alluring-allegory.bg-layer
        :glyphs)
  (:export :Story :Last-Scene :Current-Scene :Scene-Data :Scene-Data-Populate
           :Update-Current-Scene))
(in-package :alluring-allegory.story)

;;; "story" goes here. Hacks and glory await!

;; This is where we will define the story

(defclass Story (Base)
  ((Title
    :accessor Story-Title
    :initarg :title
    :initform "The title of our story")
   (Last-Scene
    :accessor Last-Scene
    :initarg :last-scene
    :initform (make-instance 'Scene :title "Introduction" :text "Welcome to the story!"))
   (Current-Scene
    :accessor Current-Scene
    :initarg :current-scene
    :initform (make-instance 'Scene :title "Introduction" :text "Welcome to the story!"))
   (Scene-Data
    :accessor Scene-Data
    :initarg :scene-data
    :initform (make-hash-table :test #'equal)))
  (:documentation "the story"))

(defgeneric Update-Current-Scene (object scene-id)
  (:documentation "Change from the current scene"))

(defmethod Update-Current-Scene ((story Story) scene-id)
  (setf (Last-Scene story) (Current-Scene story))
  (setf (Current-Scene story) (gethash scene-id (Scene-Data story))))

(defgeneric Scene-Data-Populate (object)
  (:documentation "Begin the story."))

(defun get-scene-data-list ()
  "Build a list, then convert to a hash."
  (list
   (make-instance
    'Scene
    :title "Prologue"
    :text "Move the text to the edge of the screen to make a selection."
    :choices (vector
              (make-instance 'Choice :text "The Expiriment" :next-scene "The Expiriment")
              (make-instance 'Choice :text "The Expiriment" :next-scene "The Expiriment")
              (make-instance 'Choice :text "The Expiriment" :next-scene "The Expiriment")
              (make-instance 'Choice :text "The Expiriment" :next-scene "The Expiriment")
              )
    :background (make-instance 'BG-Layer :source-image "dark.png")
    :actors (vector (make-instance 'Actor :source-image "dark.png"))
    :show-bubble-p nil
    )

   (make-instance
    'Scene
    :title "The Expiriment"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0001"))
    :text "THE EXPERIMENT"
    )

   (make-instance
    'Scene
    :title "te0001"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0010"))
    :show-bubble-p t
    :actors (vector (make-instance 'Actor :source-image "pink-hair-gs.png"))
    :background (make-instance 'BG-Layer :source-image "beach-sketch.png")
    :text "'I am glad you came, Clarke very glad indeed.  I was not sure you
could spare the time.'"
    )

   (make-instance
    'Scene
    :title "te0010"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0020"))
    :show-bubble-p t
    :actors (vector (make-instance 'Actor :source-image "pink-hair-gs.png"))
    :background (make-instance 'BG-Layer :source-image "beach-sketch.png")
    :text "'I was able to make arrangements for a few days things are not very
lively just now.  But have you no misgivings, Raymond?  Is it
absolutely safe?'"
    )

   (make-instance
    'Scene
    :title "te0020"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0030"))
    :text "The two men were slowly pacing the terrace in front of Dr. Raymond's
house.  The sun still hung above the western mountain-line, but it
shone with a dull red glow that cast no shadows, and all the air was
quiet a sweet breath came from the great wood on the hillside above,
and with it, at intervals, the soft murmuring call of the wild doves.
Below, in the long lovely valley, the river wound in and out between
the lonely hills, and, as the sun hovered and vanished into the west, a
faint mist, pure white, began to rise from the hills.  Dr. Raymond
turned sharply to his friend."
    )

   (make-instance
    'Scene
    :title "te0030"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0040"))
    :text "'Safe?  Of course it is.  In itself the operation is a perfectly simple
one any surgeon could do it.'"
    )

   (make-instance
    'Scene
    :title "te0040"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0050"))
    :text "'And there is no danger at any other stage?'"
    )

   (make-instance
    'Scene
    :title "te0050"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0060"))
    :text "'None absolutely no physical danger whatsoever, I give you my word.
You are always timid, Clarke, always but you know my history.  I have
devoted myself to transcendental medicine for the last twenty years.  I
have heard myself called quack and charlatan and impostor, but all the
while I knew I was on the right path.  Five years ago I reached the
goal, and since then every day has been a preparation for what we shall
do tonight.'"
    )

   (make-instance
    'Scene
    :title "te0060"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0070"))
    :text "'I should like to believe it is all true.'  Clarke knit his brows, and
looked doubtfully at Dr. Raymond.  'Are you perfectly sure, Raymond,
that your theory is not a phantasmagoria--a splendid vision, certainly,
but a mere vision after all?'"
    )

   (make-instance
    'Scene
    :title "te0070"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0080"))
    :text "Dr. Raymond stopped in his walk and turned sharply. He was a
middle-aged man, gaunt and thin, of a pale yellow complexion, but as he
answered Clarke and faced him, there was a flush on his cheek."
    )

   (make-instance
    'Scene
    :title "te0080"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0090"))
    :text "'Look about you, Clarke.  You see the mountain, and hill following
after hill, as wave on wave, you see the woods and orchard, the fields
of ripe corn, and the meadows reaching to the reed-beds by the river.
You see me standing here beside you, and hear my voice but I tell you
that all these things--yes, from that star that has just shone out in
the sky to the solid ground beneath our feet--I say that all these are
but dreams and shadows the shadows that hide the real world from our
eyes."
    )

   (make-instance
    'Scene
    :title "te0090"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0100"))
    :text
    "There is a real world, but it is beyond this glamour and this
vision, beyond these 'chases in Arras, dreams in a career,' beyond them
all as beyond a veil.  I do not know whether any human being has ever
lifted that veil but I do know, Clarke, that you and I shall see it
lifted this very night from before another's eyes.  You may think this
all strange nonsense it may be strange, but it is true, and the
ancients knew what lifting the veil means.  They called it seeing the
god Pan.'"
    )

   (make-instance
    'Scene
    :title "te0100"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0110"))
    :text "Clarke shivered the white mist gathering over the river was chilly."
    )

   (make-instance
    'Scene
    :title "te0110"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0120"))
    :text "'It is wonderful indeed,' he said.  'We are standing on the brink of a
strange world, Raymond, if what you say is true.  I suppose the knife
is absolutely necessary?'"
    )

   (make-instance
    'Scene
    :title "te0120"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0130"))
    :text "'Yes a slight lesion in the grey matter, that is all a trifling
rearrangement of certain cells, a microscopical alteration that would
escape the attention of ninety-nine brain specialists out of a hundred.
I don't want to bother you with 'shop,' Clarke I might give you a mass
of technical detail which would sound very imposing, and would leave
you as enlightened as you are now."
    )

   (make-instance
    'Scene
    :title "te0130"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0140"))
    :text "
      But I suppose you have read,
casually, in out-of-the-way corners of your paper, that immense strides
have been made recently in the physiology of the brain.  I saw a
paragraph the other day about Digby's theory, and Browne Faber's
discoveries.  Theories and discoveries!  Where they are standing now, I
stood fifteen years ago, and I need not tell you that I have not been
standing still for the last fifteen years.  "
    )

   (make-instance
    'Scene
    :title "te0140"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0150"))
    :text "
      It will be enough if I say
that five years ago I made the discovery that I alluded to when I said
that ten years ago I reached the goal.  After years of labour, after
years of toiling and groping in the dark, after days and nights of
disappointments and sometimes of despair, in which I used now and then
to tremble and grow cold with the thought that perhaps there were
others seeking for what I sought, at last, after so long, a pang of
sudden joy thrilled my soul, and I knew the long journey was at an end."
    )

   (make-instance
    'Scene
    :title "te0150"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0160"))
    :text "
By what seemed then and still seems a chance, the suggestion of a
moment's idle thought followed up upon familiar lines and paths that I
had tracked a hundred times already, the great truth burst upon me, and
I saw, mapped out in lines of sight, a whole world, a sphere unknown
continents and islands, and great oceans in which no ship has sailed
to my belief since a Man first lifted up his eyes and beheld the sun,
and the stars of heaven, and the quiet earth beneath.  You will think
this all high-flown language, Clarke, but it is hard to be literal.
And yet ")

   (make-instance
    'Scene
    :title "te0160"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0170"))
    :text "
I do not know whether what I am hinting at cannot be set forth
in plain and lonely terms.  For instance, this world of ours is pretty
well girded now with the telegraph wires and cables thought, with
something less than the speed of thought, flashes from sunrise to
sunset, from north to south, across the floods and the desert places.
Suppose that an electrician of today were suddenly to perceive that he
and his friends have merely been playing with pebbles and mistaking
them for the foundations of the world "
    )

   (make-instance
    'Scene
    :title "te0170"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0180"))
    :text "suppose that such a man saw
uttermost space lie open before the current, and words of men flash
forth to the sun and beyond the sun into the systems beyond, and the
voice of articulate-speaking men echo in the waste void that bounds our
thought.  As analogies go, that is a pretty good analogy of what I have
done- you can understand now a little of what I felt as I stood here
one evening "
    )

   (make-instance
    'Scene
    :title "te0180"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0190"))
    :text "
it was a summer evening, and the valley looked much as it
does now- I stood here, and saw before me the unutterable, the
unthinkable gulf that yawns profound between two worlds, the world of
matter and the world of spirit- "
    )

   (make-instance
    'Scene
    :title "te0190"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0200"))
    :text "
I saw the great empty deep stretch dim
before me, and in that instant a bridge of light leapt from the earth
to the unknown shore, and the abyss was spanned."
    )

   (make-instance
    'Scene
    :title "te0200"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0210"))
    :text "
You may look in
Browne Faber's book, if you like, and you will find that to the present
day men of science are unable to account for the presence, or to
specify the functions of a certain group of nerve-cells in the brain.
That group is, as it were, land to let, a mere waste place for fanciful
theories.  "
    )

   (make-instance
    'Scene
    :title "te0210"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0220"))
    :text "
I am not in the position of Browne Faber and the
specialists, I am perfectly instructed as to the possible functions of
those nerve-centers in the scheme of things.  With a touch I can bring
them into play, with a touch, I say, I can set free the current, with a
touch I can complete the communication between this world of sense
and--we shall be able to finish the sentence later on."
    )

   (make-instance
    'Scene
    :title "te0220"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0230"))
    :text "
Yes, the knife
is necessary- but think what that knife will effect.  It will level
utterly the solid wall of sense, and probably, for the first time since
man was made, a spirit will gaze on a spirit-world.  Clarke, Mary will
see the god Pan!'"
    )

   (make-instance
    'Scene
    :title "te0230"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0240"))
    :text "'But you remember what you wrote to me?  I thought it would be
requisite that she--'"
    )

   (make-instance
    'Scene
    :title "te0240"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0250"))
    :text "He whispered the rest into the doctor's ear."
    )

   (make-instance
    'Scene
    :title "te0250"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0260"))
    :text "'Not at all, not at all.  That is nonsense.  I assure you.  Indeed, it
is better as it is- I am quite certain of that.'"
    )

   (make-instance
    'Scene
    :title "te0260"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0270"))
    :text "'Consider the matter well, Raymond.  It's a great responsibility.
Something might go wrong- you would be a miserable man for the rest of
your days.'"
    )

   (make-instance
    'Scene
    :title "te0270"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0280"))
    :text "'No, I think not, even if the worst happened.  As you know, I rescued
Mary from the gutter, and from almost certain starvation, when she was
a child- I think her life is mine, to use as I see fit.  Come, it's
getting late- we had better go in.'"
    )

   (make-instance
    'Scene
    :title "te0280"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0290"))
    :text "Dr. Raymond led the way into the house, through the hall, and down a
long dark passage.  He took a key from his pocket and opened a heavy
door, and motioned Clarke into his laboratory.  It had once been a
billiard-room, and was lighted by a glass dome in the centre of the
ceiling, whence there still shone a sad grey light on the figure of the
doctor as he lit a lamp with a heavy shade and placed it on a table in
the middle of the room."
    )

   (make-instance
    'Scene
    :title "te0290"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0300"))
    :text "Clarke looked about him.  Scarcely a foot of wall remained bare- there
were shelves all around laden with bottles and phials of all shapes and
colours, and at one end stood a little Chippendale book-case.  Raymond
pointed to this."
    )

   (make-instance
    'Scene
    :title "te0300"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0310"))
    :text "'You see that parchment Oswald Crollius?  He was one of the first to
show me the way, though I don't think he ever found it himself.  That
is a strange saying of his: 'In every grain of wheat there lies hidden
the soul of a star.''"
    )

   (make-instance
    'Scene
    :title "te0310"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0320"))
    :text "There was not much furniture in the laboratory.  The table in the
centre, a stone slab with a drain in one corner, the two armchairs on
which Raymond and Clarke were sitting- that was all, except an
odd-looking chair at the furthest end of the room.  Clarke looked at
it, and raised his eyebrows."
    )

   (make-instance
    'Scene
    :title "te0320"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0330"))
    :text "'Yes, that is the chair,' said Raymond.  'We may as well place it in
position.'  He got up and wheeled the chair to the light, and began
raising and lowering it, letting down the seat, setting the back at
various angles, and adjusting the foot-rest.  It looked comfortable
enough, and Clarke passed his hand over the soft green velvet, as the
doctor manipulated the levers."
    )

   (make-instance
    'Scene
    :title "te0330"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0340"))
    :text "'Now, Clarke, make yourself quite comfortable.  I have a couple hours'
work before me- I was obliged to leave certain matters to the last.'"
    )

   (make-instance
    'Scene
    :title "te0340"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0350"))
    :text "Raymond went to the stone slab, and Clarke watched him drearily as he
bent over a row of phials and lit the flame under the crucible.  The
doctor had a small hand-lamp, shaded as the larger one, on a ledge
above his apparatus, and Clarke, who sat in the shadows, looked down at
the great shadowy room, wondering at the bizarre effects of brilliant
light and undefined darkness contrasting with one another."
    )

   (make-instance
    'Scene
    :title "te0350"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0360"))
    :text
    "Soon he
became conscious of an odd odour, at first the merest suggestion of
odour, in the room, and as it grew more decided he felt surprised that
he was not reminded of the chemist's shop or the surgery.  Clarke found
himself idly endeavouring to analyse the sensation, and half conscious,
he began to think of a day, fifteen years ago, that he had spent
roaming through the woods and meadows near his own home.  "
    )

   (make-instance
    'Scene
    :title "te0360"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0370"))
    :text
   "It was a
burning day at the beginning of August, the heat had dimmed the
outlines of all things and all distances with a faint mist, and people
who observed the thermometer spoke of an abnormal register, of a
temperature that was almost tropical. Strangely that wonderful hot day
of the fifties rose up again in Clarke's imagination- the sense of
dazzling all-pervading sunlight seemed to blot out the shadows and the
lights of the laboratory, and he felt again the heated air beating in
gusts about his face, saw the shimmer rising from the turf, and heard
the myriad murmur of the summer."
    )

   (make-instance
    'Scene
    :title "te0370"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0380"))
    :text "'I hope the smell doesn't annoy you, Clarke- there's nothing
unwholesome about it.  It may make you a bit sleepy, that's all.'"
    )

   (make-instance
    'Scene
    :title "te0380"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0390"))
    :text "Clarke heard the words quite distinctly, and knew that Raymond was
speaking to him, but for the life of him he could not rouse himself
from his lethargy.  He could only think of the lonely walk he had taken
fifteen years ago- it was his last look at the fields and woods he had
known since he was a child, and now it all stood out in brilliant
light, as a picture, before him.  "
    )

   (make-instance
    'Scene
    :title "te0390"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0400"))
    :text "
   Above all there came to his nostrils
the scent of summer, the smell of flowers mingled, and the odour of the
woods, of cool shaded places, deep in the green depths, drawn forth by
the sun's heat- and the scent of the good earth, lying as it were with
arms stretched forth, and smiling lips, overpowered all. His fancies
made him wander, as he had wandered long ago, from the fields into the
wood, tracking a little path between the shining undergrowth of
beech-trees- and the trickle of water dropping from the limestone rock
sounded as a clear melody in the dream."
    )
   (make-instance
    'Scene
    :title "te0400"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0410"))
    :text "
   Thoughts began to go astray
and to mingle with other thoughts- the beech alley was transformed to a
path between ilex-trees, and here and there a vine climbed from bough
to bough, and sent up waving tendrils and drooped with purple grapes,
and the sparse grey-green leaves of a wild olive-tree stood out against
the dark shadows of the ilex.  "
    )

   (make-instance
    'Scene
    :title "te0410"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0420"))
    :text "
   Clarke, in the deep folds of dream, was
conscious that the path from his father's house had led him into an
undiscovered country, and he was wondering at the strangeness of it
all, when suddenly, in place of the hum and murmur of the summer, an
infinite silence seemed to fall on all things, and the wood was hushed,
and for a moment in time he stood face to face there with a presence,
that was neither man nor beast, neither the living nor the dead, but
all things mingled, the form of all things but devoid of all form.  And
in that moment, the sacrament of body and soul was dissolved, and a
voice seemed to cry 'Let us go hence,' and then the darkness of
darkness beyond the stars, the darkness of everlasting."
    )

   (make-instance
    'Scene
    :title "te0420"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0430"))
    :text "When Clarke woke up with a start he saw Raymond pouring a few drops of
some oily fluid into a green phial, which he stoppered tightly."
    )

   (make-instance
    'Scene
    :title "te0430"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0440"))
    :text "'You have been dozing,' he said- 'the journey must have tired you out.
It is done now.  I am going to fetch Mary- I shall be back in ten
minutes.'"
    )

   (make-instance
    'Scene
    :title "te0440"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0450"))
    :text "Clarke lay back in his chair and wondered.  It seemed as if he had but
passed from one dream into another.  He half expected to see the walls
of the laboratory melt and disappear, and to awake in London,
shuddering at his own sleeping fancies. But at last the door opened,
and the doctor returned, and behind him came a girl of about seventeen,
dressed all in white.  She was so beautiful that Clarke did not wonder
at what the doctor had written to him.  She was blushing now over face
and neck and arms, but Raymond seemed unmoved."
    )

   (make-instance
    'Scene
    :title "te0450"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0460"))
    :text "'Mary,' he said, 'the time has come.  You are quite free.  Are you
willing to trust yourself to me entirely?'"
    )

   (make-instance
    'Scene
    :title "te0460"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0470"))
    :text "'Yes, dear.'"
    )

   (make-instance
    'Scene
    :title "te0470"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0480"))
    :text "'Do you hear that, Clarke?  You are my witness.  Here is the chair,
Mary.  It is quite easy.  Just sit in it and lean back.  Are you ready?'"
    )

   (make-instance
    'Scene
    :title "te0480"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0490"))
    :text "'Yes, dear, quite ready.  Give me a kiss before you begin.'"
    )

   (make-instance
    'Scene
    :title "te0490"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0500"))
    :text "The doctor stooped and kissed her mouth, kindly enough. 'Now shut your
eyes,' he said.  The girl closed her eyelids, as if she were tired, and
longed for sleep, and Raymond placed the green phial to her nostrils.
Her face grew white, whiter than her dress- she struggled faintly, and
then with the feeling of submission strong within her, crossed her arms
upon her breast as a little child about to say her prayers.  "
    )

   (make-instance
    'Scene
    :title "te0500"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0510"))
    :text "
   The bright
light of the lamp fell full upon her, and Clarke watched changes
fleeting over her face as the changes of the hills when the summer
clouds float across the sun.  And then she lay all white and still, and
the doctor turned up one of her eyelids.  She was quite unconscious.
Raymond pressed hard on one of the levers and the chair instantly sank
back.  Clarke saw him cutting away a circle, like a tonsure, from her
hair, and the lamp was moved nearer.  Raymond took a small glittering
instrument from a little case, and Clarke turned away shudderingly.
When he looked again the doctor was binding up the wound he had made."
    )

   (make-instance
    'Scene
    :title "te0510"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0520"))
    :text "'She will awake in five minutes.'  Raymond was still perfectly cool.
'There is nothing more to be done- we can only wait.'"
    )

   (make-instance
    'Scene
    :title "te0520"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0530"))
    :text "The minutes passed slowly- they could hear a slow, heavy, ticking.
There was an old clock in the passage.  Clarke felt sick and faint- his
knees shook beneath him, he could hardly stand."
    )

   (make-instance
    'Scene
    :title "te0530"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0540"))
    :text "Suddenly, as they watched, they heard a long-drawn sigh, and suddenly
did the colour that had vanished return to the girl's cheeks, and
suddenly her eyes opened.  Clarke quailed before them.  They shone with
an awful light, looking far away, and a great wonder fell upon her
face, and her hands stretched out as if to touch what was invisible-"
    )

   (make-instance
    'Scene
    :title "te0540"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0550"))
    :text "but in an instant the wonder faded, and gave place to the most awful
terror.  The muscles of her face were hideously convulsed, she shook
from head to foot- the soul seemed struggling and shuddering within the
house of flesh.  It was a horrible sight, and Clarke rushed forward, as
she fell shrieking to the floor."
    )

   (make-instance
    'Scene
    :title "te0550"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0560"))
    :text "Three days later Raymond took Clarke to Mary's bedside. She was lying
wide-awake, rolling her head from side to side, and grinning vacantly."
    )

   (make-instance
    'Scene
    :title "te0560"
    :choices (vector (make-instance 'Choice :text "Next" :next-scene "te0560"))
    :text "'Yes,' said the doctor, still quite cool, 'it is a great pity- she is a
hopeless idiot.  However, it could not be helped- and, after all, she
has seen the Great God Pan.'"
    )
   ))

(defmethod Scene-Data-Populate ((story Story))
  "Build a list, then convert to a hash."
  (loop for scene in (get-scene-data-list)
     do (setf (gethash (Title scene) (Scene-data story)) scene)))
