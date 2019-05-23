/* CMiniTreeIndividual.cs
 * 
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
 *
 *
 * History:  
 * 10Dec08 AlexC          Migrated from GEDmill 1.10
 *
 */

using System;
using System.Collections.Generic;
using System.Drawing;
using GDModel;

namespace GEDmill.MiniTree
{
    /// <summary>
    /// Creates the boxes in the mini tree for each individual.
    /// Each box consists of text surrouned by PADDING surrouned by a single gedcomLine
    /// BORDER, surrounded by a MARGIN. (Same terminology as CSS box model.)
    /// </summary>
    public class MiniTreeIndividual : MiniTreeObject
    {
        // Horizontal padding between boxes
        // Half size because margins don't collapse here
        private const float MARGIN_HORIZ = 4.0f;

        // Vertical padding between boxes
        // Half size because margins don't collapse here
        private const float MARGIN_VERT = 8.0f;

        // Horizontal padding around the text
        private const float PADDING_HORIZ = 4.0f;

        // Vertical padding around the text
        private const float PADDING_VERT = 4.0f;

        // The individual that this box represents
        private GDMIndividualRecord fIndiRec;

        // Whether this box can be clicked to link to the individual record page
        private bool fLinkable;

        // Whether this box has parent boxes in the diagram
        private bool fChild;

        // Whether the box should be painted highlighted
        private bool fHighlight;

        // Whether the box is for a concealed record 
        private bool fConcealed;

        // Whether the box should be painted in the shade colour
        private bool fShade;

        // Text for the first names
        private string fFirstnames;

        // Text for the surname
        private string fSurname;

        // Text for the date
        private string fDate;

        // Amount of horizontal offset required to put firstnames in centre
        private float fFirstnamesPad;

        // Amount of horizontal offset required to put surname in centre
        private float fSurnamePad;

        // Amount of horizontal offset required to put date in centre
        private float fDatePad;

        // If true, name will be split over 2 lines to make boxes taller and narrower.
        private bool fConserveWidth;

        // X coordinate of this box's position in diagram
        private float fX;

        // Y coordinate of this box's position in diagram
        private float fY;

        // Total size of this box
        private SizeF fSize;

        // Total size of rectangular hull containing all the text in the box
        private SizeF fSizeText;

        // Size of the text required by the first names (if the conserve-width option is on)
        private SizeF fSizeFirstnames;

        // Size of the text required by the surname (if the conserve-width option is on)
        private SizeF fSizeSurname;

        // Size of the date text
        private SizeF fSizeDate;


        // Accessor for the individual's name
        private string Name
        {
            get { return string.Concat(fFirstnames, " ", fSurname); }
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
            get { return fY + MARGIN_VERT + (fSizeText.Height / 2f); }
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
            get { return fChild; }
        }


        public MiniTreeIndividual(GDMIndividualRecord ir, string firstNames, string surname, string date,
                                  bool createLink, bool createStalk, bool highlight, bool concealed, bool shade,
                                  bool conserveWidth)
        {
            fIndiRec = ir;
            fFirstnames = firstNames;
            fSurname = surname;
            fDate = date;
            fLinkable = createLink;
            fHighlight = highlight;
            fConcealed = concealed;
            fChild = createStalk;
            fShade = shade;
            fFirstnamesPad = 0f;
            fSurnamePad = 0f;
            fDatePad = 0f;
            fConserveWidth = conserveWidth;
            fSizeText = new Size();
        }

        // Calculates the size required by this box. Initialises the class fields 
        // that contain size information. Returns the overall box size.
        public override SizeF CalculateSize(Graphics g, Font f)
        {
            fSizeDate = g.MeasureString(fDate, f);
            if (fConserveWidth) {
                fSizeFirstnames = g.MeasureString(fFirstnames, f);
                fSizeSurname = g.MeasureString(fSurname, f);
            } else {
                fSizeFirstnames.Height = 0;
                fSizeFirstnames.Width = 0;
                fSizeSurname = g.MeasureString(Name, f);
            }

            if (fSizeFirstnames.Width > fSizeSurname.Width) {
                if (fSizeDate.Width > fSizeFirstnames.Width) {
                    fSizeText.Width = fSizeDate.Width;
                    fDatePad = 0f;
                    fFirstnamesPad = (fSizeDate.Width - fSizeFirstnames.Width) / 2f;
                    fSurnamePad = (fSizeDate.Width - fSizeSurname.Width) / 2f;
                } else {
                    fSizeText.Width = fSizeFirstnames.Width;
                    fDatePad = (fSizeFirstnames.Width - fSizeDate.Width) / 2f;
                    fFirstnamesPad = 0f;
                    fSurnamePad = (fSizeFirstnames.Width - fSizeSurname.Width) / 2f;
                }
            } else {
                if (fSizeDate.Width > fSizeSurname.Width) {
                    fSizeText.Width = fSizeDate.Width;
                    fDatePad = 0f;
                    fFirstnamesPad = (fSizeDate.Width - fSizeFirstnames.Width) / 2f;
                    fSurnamePad = (fSizeDate.Width - fSizeSurname.Width) / 2f;
                } else {
                    fSizeText.Width = fSizeSurname.Width;
                    fDatePad = (fSizeSurname.Width - fSizeDate.Width) / 2f;
                    fFirstnamesPad = (fSizeSurname.Width - fSizeFirstnames.Width) / 2f;
                    fSurnamePad = 0f;
                }
            }
            fSizeText.Height = fSizeFirstnames.Height + fSizeSurname.Height + fSizeDate.Height;

            fSizeText.Width += PADDING_HORIZ * 2f;
            fSizeText.Height += PADDING_VERT * 2f;

            fSize = fSizeText;
            fSize.Width += MARGIN_HORIZ * 2f;
            fSize.Height += MARGIN_VERT * 2f;

            return fSize;
        }

        // Draws the actual box, and adds the region of the box to the image alMap list.
        public override void DrawBitmap(Paintbox paintbox, Graphics g, List<MiniTreeMap> map)
        {
            SolidBrush solidbrushBg, solidbrushText;

            if (fConcealed) {
                solidbrushBg = paintbox.BrushBoxConcealed;
            } else if (fHighlight) {
                solidbrushBg = paintbox.BrushBoxHighlight;
            } else if (fShade) {
                solidbrushBg = paintbox.BrushBoxShade;
            } else {
                solidbrushBg = paintbox.BrushBox;
            }

            if (fLinkable) {
                solidbrushText = paintbox.BrushTextLink;
            } else if (fConcealed) {
                solidbrushText = paintbox.BrushTextConcealed;
            } else {
                solidbrushText = paintbox.BrushText;
            }

            g.FillRectangle(solidbrushBg, fX + MARGIN_HORIZ, fY + MARGIN_VERT, fSizeText.Width, fSizeText.Height - 1f);
            g.DrawRectangle(paintbox.PenBox, fX + MARGIN_HORIZ, fY + MARGIN_VERT, fSizeText.Width, fSizeText.Height - 1f);

            float fTextX = fX + MARGIN_HORIZ + PADDING_HORIZ;
            float fTextY = fY + MARGIN_VERT + PADDING_VERT;
            if (fConserveWidth) {
                g.DrawString(fFirstnames, paintbox.Font, solidbrushText, fTextX + fFirstnamesPad, fTextY);
                fTextY += fSizeFirstnames.Height;
                g.DrawString(fSurname, paintbox.Font, solidbrushText, fTextX + fSurnamePad, fTextY);
                fTextY += fSizeSurname.Height;
            } else {
                g.DrawString(Name, paintbox.Font, solidbrushText, fTextX + fSurnamePad, fTextY);
                fTextY += fSizeSurname.Height;
            }

            g.DrawString(fDate, paintbox.Font, solidbrushText, fTextX + fDatePad, fTextY);

            if (fChild) {
                g.DrawLine(paintbox.PenConnector, fX + MARGIN_HORIZ + (fSizeText.Width / 2f), fY, fX + MARGIN_HORIZ + (fSizeText.Width / 2f), fY + MARGIN_VERT/* -1f*/ );
            }

            if (fIndiRec != null) {
                map.Add(new MiniTreeMap(Name, fIndiRec, fLinkable,
                    (int)(fX + MARGIN_HORIZ), (int)(fY + MARGIN_VERT),
                    (int)(fX + MARGIN_HORIZ + fSizeText.Width), (int)(fY + MARGIN_VERT + fSizeText.Height - 1f)));
            }
        }

        // Sets the location of this box. Size should have already been
        // calculated by now.
        public override SizeF CalculateLayout(float x, float y)
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
                if (RightObject is MiniTreeGroup) {
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
                if (LeftObject is MiniTreeGroup) {
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
                if (LeftObjectAlien is MiniTreeIndividual) {
                    // Push left until hit alien object
                    float distance = Left - ((MiniTreeIndividual)LeftObjectAlien).Right;
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
                if (RightObjectAlien is MiniTreeIndividual) {
                    // Push right until hit alien object
                    float distance = ((MiniTreeIndividual)RightObjectAlien).Left - Right;
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
