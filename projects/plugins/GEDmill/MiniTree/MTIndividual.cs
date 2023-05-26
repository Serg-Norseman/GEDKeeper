/* 
 * Copyright 2009 Alexander Curtis <alex@logicmill.com>
 * This file is part of GEDmill - A family history website creator
 * 
 * GEDmill is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GEDmill is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GEDmill.  If not, see <http://www.gnu.org/licenses/>.
 */

using BSLib;
using GDModel;

namespace GEDmill.MiniTree
{
    /// <summary>
    /// Creates the boxes in the mini tree for each individual.
    /// Each box consists of text surrouned by PADDING surrouned by a single gedcomLine
    /// BORDER, surrounded by a MARGIN. (Same terminology as CSS box model.)
    /// </summary>
    public class MTIndividual : MTObject
    {
        // Horizontal padding between boxes
        // Half size because margins don't collapse here
        public const float MARGIN_HORIZ = 4.0f;

        // Vertical padding between boxes
        // Half size because margins don't collapse here
        public const float MARGIN_VERT = 8.0f;

        // Horizontal padding around the text
        public const float PADDING_HORIZ = 4.0f;

        // Vertical padding around the text
        public const float PADDING_VERT = 4.0f;

        private GDMIndividualRecord fIndiRec;
        private ExtSizeF fSizeFirstnames;
        private ExtSizeF fSizeSurname;
        private ExtSizeF fSizeText;

        // The individual that this box represents
        public GDMIndividualRecord IndiRec { get { return fIndiRec; } }

        // Whether this box can be clicked to link to the individual record page
        public bool Linkable { get; private set; }

        // Whether this box has parent boxes in the diagram
        public bool Child { get; private set; }

        // Whether the box should be painted highlighted
        public bool Highlight { get; private set; }

        // Whether the box is for a concealed record 
        public bool Concealed { get; private set; }

        // Whether the box should be painted in the shade color
        public bool Shade { get; private set; }

        // Text for the first names
        public string Firstnames { get; private set; }

        // Text for the surname
        public string Surname { get; private set; }

        // Text for the date
        public string Date { get; private set; }

        // Amount of horizontal offset required to put firstnames in centre
        public float FirstnamesPad { get; private set; }

        // Amount of horizontal offset required to put surname in centre
        public float SurnamePad { get; private set; }

        // Amount of horizontal offset required to put date in centre
        public float DatePad { get; private set; }

        // If true, name will be split over 2 lines to make boxes taller and narrower.
        public bool ConserveWidth { get; private set; }

        // X coordinate of this box's position in diagram
        private float fX;

        // Y coordinate of this box's position in diagram
        private float fY;

        // Total size of this box
        private ExtSizeF fSize;

        // Total size of rectangular hull containing all the text in the box
        public ExtSizeF SizeText { get { return fSizeText; } }

        // Size of the text required by the first names (if the conserve-width option is on)
        public ExtSizeF SizeFirstnames { get { return fSizeFirstnames; } }

        // Size of the text required by the surname (if the conserve-width option is on)
        public ExtSizeF SizeSurname { get { return fSizeSurname; } }

        // Size of the date text
        private ExtSizeF fSizeDate;


        // Accessor for the individual's name
        public string Name
        {
            get { return string.Concat(Firstnames, " ", Surname); }
        }

        // Accessor for left side position of box sArea.
        public float Left
        {
            get { return fX; }
        }

        // Accessor for right side position of box sArea.
        public float Right
        {
            get { return fX + fSize.Width; }
        }

        // Accessor for top side position of box sArea.
        public float Top
        {
            get { return fY; }
        }

        // Accessor for bottom side position of box sArea.
        public float Bottom
        {
            get { return fY + fSize.Height; }
        }

        // The vertical centre of the box, where the tee gedcomLine between boxes should attach.
        public float TeeCentreVert
        {
            get { return fY + MARGIN_VERT + (SizeText.Height / 2f); }
        }

        // The left edge of the box, where the tee gedcomLine between boxes should attach.
        public float TeeLeft
        {
            get { return fX + MARGIN_HORIZ; }
        }

        // The right edge of the box, where the tee gedcomLine between boxes should attach.
        public float TeeRight
        {
            get { return fX + fSize.Width - MARGIN_HORIZ; }
        }

        // Caller should call HasStalk first to ensure nResult from this property is valid.
        // The horizontal centre of the box, where the tee gedcomLine up to frParents should attach.
        public float Stalk
        {
            get { return fX + (fSize.Width / 2f); }
        }

        // Returns true if this box needs a gedcomLine drawing up to a parent.
        public bool HasStalk
        {
            get { return Child; }
        }


        public MTIndividual(ITreeDrawer drawer,
                                  GDMIndividualRecord ir, string firstNames, string surname, string date,
                                  bool createLink, bool createStalk, bool highlight, bool concealed, bool shade,
                                  bool conserveWidth) : base(drawer)
        {
            fIndiRec = ir;
            Firstnames = firstNames;
            Surname = surname;
            Date = date;
            Linkable = createLink;
            Highlight = highlight;
            Concealed = concealed;
            Child = createStalk;
            Shade = shade;
            FirstnamesPad = 0f;
            SurnamePad = 0f;
            DatePad = 0f;
            ConserveWidth = conserveWidth;
            fSizeText = new ExtSize();
        }

        // Calculates the size required by this box. Initialises the class fields 
        // that contain size information. Returns the overall box size.
        public override ExtSizeF CalculateSize()
        {
            fSizeDate = fDrawer.MeasureString(Date);
            if (ConserveWidth) {
                fSizeFirstnames = fDrawer.MeasureString(Firstnames);
                fSizeSurname = fDrawer.MeasureString(Surname);
            } else {
                fSizeFirstnames.Height = 0;
                fSizeFirstnames.Width = 0;
                fSizeSurname = fDrawer.MeasureString(Name);
            }

            if (fSizeFirstnames.Width > SizeSurname.Width) {
                if (fSizeDate.Width > fSizeFirstnames.Width) {
                    fSizeText.Width = fSizeDate.Width;
                    DatePad = 0f;
                    FirstnamesPad = (fSizeDate.Width - fSizeFirstnames.Width) / 2f;
                    SurnamePad = (fSizeDate.Width - SizeSurname.Width) / 2f;
                } else {
                    fSizeText.Width = fSizeFirstnames.Width;
                    DatePad = (fSizeFirstnames.Width - fSizeDate.Width) / 2f;
                    FirstnamesPad = 0f;
                    SurnamePad = (fSizeFirstnames.Width - SizeSurname.Width) / 2f;
                }
            } else {
                if (fSizeDate.Width > SizeSurname.Width) {
                    fSizeText.Width = fSizeDate.Width;
                    DatePad = 0f;
                    FirstnamesPad = (fSizeDate.Width - fSizeFirstnames.Width) / 2f;
                    SurnamePad = (fSizeDate.Width - SizeSurname.Width) / 2f;
                } else {
                    fSizeText.Width = SizeSurname.Width;
                    DatePad = (SizeSurname.Width - fSizeDate.Width) / 2f;
                    FirstnamesPad = (SizeSurname.Width - fSizeFirstnames.Width) / 2f;
                    SurnamePad = 0f;
                }
            }
            fSizeText.Height = fSizeFirstnames.Height + SizeSurname.Height + fSizeDate.Height;

            fSizeText.Width += PADDING_HORIZ * 2f;
            fSizeText.Height += PADDING_VERT * 2f;

            fSize = fSizeText;
            fSize.Width += MARGIN_HORIZ * 2f;
            fSize.Height += MARGIN_VERT * 2f;

            return fSize;
        }

        // Sets the location of this box. Size should have already been
        // calculated by now.
        public override ExtSizeF CalculateLayout(float x, float y)
        {
            fX = x;
            fY = y;

            return fSize;
        }

        // Moves this object left and all objects to its right left, until one of them
        // can't move without breaking the diagram.
        // Returns amount actually moved, which may be less than amount asked for.
        public override float PullLeft(float amount)
        {
            if (RightObject != null) {
                if (RightObject is MTGroup) {
                    // Groups are stretchy so we can ignore if they get stuck (and hence reduce 'amount')
                    RightObject.PullLeft(amount);
                } else {
                    amount = RightObject.PullLeft(amount);
                }
            }
            if (amount <= 0f) {
                // Nothing to do
                return amount;
            }

            // Individuals are free to move unless anchored by an attached group.
            fX -= amount;

            return amount;
        }

        // Moves this object right and all objects to its left right, until one of them
        // can't move without breaking the diagram.
        // Returns amount actually moved, which may be less than amount asked for.
        public override float PullRight(float amount)
        {
            if (LeftObject != null) {
                if (LeftObject is MTGroup) {
                    // Groups are stretchy so we can ignore if they 
                    // get stuck( and hence reduce 'amount')
                    LeftObject.PullRight(amount);
                } else {
                    amount = LeftObject.PullRight(amount);
                }
            }
            if (amount <= 0f) {
                // Nothing to do
                return amount;
            }
            // Individuals are free to move unless anchored by an attached group.
            fX += amount;

            return amount;
        }

        // Moves this object left and all objects to its left left, until one of them
        // can't move without breaking the diagram.
        // Returns amount actually moved, which may be less than amount asked for.
        public override float PushLeft(float amount)
        {
            if (LeftObject != null) {
                amount = LeftObject.PushLeft(amount);
            } else if (LeftObjectAlien != null) {
                if (LeftObjectAlien is MTIndividual) {
                    // Push left until hit alien object
                    float distance = Left - ((MTIndividual)LeftObjectAlien).Right;
                    if (distance < amount) {
                        amount = distance;
                    }
                }
            }
            if (amount <= 0f) {
                // Nothing to do
                return amount;
            }
            // Individuals are free to move unless anchored by an attached group.
            fX -= amount;

            return amount;
        }

        // Moves this object right and all objects to its right right, until one of them
        // can't move without breaking the diagram.
        // Returns amount actually moved, which may be less than amount asked for.
        public override float PushRight(float amount)
        {
            if (RightObject != null) {
                amount = RightObject.PushRight(amount);
            } else if (RightObjectAlien != null) {
                if (RightObjectAlien is MTIndividual) {
                    // Push right until hit alien object
                    float distance = ((MTIndividual)RightObjectAlien).Left - Right;
                    if (distance < amount) {
                        amount = distance;
                    }
                }
            }

            if (amount <= 0f) {
                // Nothing to do
                return amount;
            }
            fX += amount; // Individuals are free to move unless anchored by a group attached

            return amount;
        }

        // Moves the position of the box by an absolute amount.
        public override void Translate(float deltaX, float deltaY)
        {
            fX += deltaX;
            fY += deltaY;
        }
    }
}
