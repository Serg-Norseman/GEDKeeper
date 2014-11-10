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
using GEDmill.LLClasses;
using System.Drawing;
using System.Collections;

namespace GEDmill.ImageClasses
{
    // Creates the boxes in the mini tree for each individual.
    // Each box consists of text surrouned by PADDING surrouned by a single gedcomLine
    // BORDER, surrounded by a MARGIN. (Same terminology as CSS box model.)
    public class CMiniTreeIndividual : CMiniTreeObject
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
        private CIndividualRecord m_ir;

        // Whether this box can be clicked to link to the individual record page
        private bool m_bLinkable;

        // Whether this box has parent boxes in the diagram
        private bool m_bChild;

        // Whether the box should be painted highlighted
        private bool m_bHighlight;

        // Whether the box is for a concealed record 
        private bool m_bConcealed;

        // Whether the box should be painted in the shade colour
        private bool m_bShade;

        // Text for the first names
        private string m_sFirstnames;

        // Text for the surname
        private string m_sSurname;

        // Text for the date
        private string m_sDate;

        // Amount of horizontal offset required to put firstnames in centre
        private float m_fFirstnamesPad;

        // Amount of horizontal offset required to put surname in centre
        private float m_fSurnamePad;

        // Amount of horizontal offset required to put date in centre
        private float m_fDatePad;

        // If true, name will be split over 2 lines to make boxes taller and narrower.
        private bool m_bConserveWidth;

        // X coordinate of this box's position in diagram
        private float m_x;

        // Y coordinate of this box's position in diagram
        private float m_y;

        // Total size of this box
        private SizeF m_size;

        // Total size of rectangular hull containing all the text in the box
        private SizeF m_sizeText;

        // Size of the text required by the first names (if the conserve-width option is on)
        private SizeF m_sizeFirstnames;

        // Size of the text required by the surname (if the conserve-width option is on)
        private SizeF m_sizeSurname;

        // Size of the date text
        private SizeF m_sizeDate;

        // Constructor
        public CMiniTreeIndividual( CIndividualRecord ir, string sFirstNames, string sSurname, string sDate, bool bCreateLink, bool bCreateStalk, bool bHighlight, bool bConcealed, bool bShade, bool bConserveWidth )
        {
            m_ir = ir;
            m_sFirstnames = sFirstNames;
            m_sSurname = sSurname;
            m_sDate = sDate;
            m_bLinkable = bCreateLink;
            m_bHighlight = bHighlight;
            m_bConcealed = bConcealed;
            m_bChild = bCreateStalk;
            m_bShade = bShade;
            m_fFirstnamesPad = 0f;
            m_fSurnamePad = 0f;
            m_fDatePad = 0f;
            m_bConserveWidth = bConserveWidth;
            m_sizeText = new Size();
        }

        // Calculates the size required by this box. Initialises the class fields 
        // that contain size information. Returns the overall box size.
        public override SizeF CalculateSize( Graphics g, Font f )
        {
            m_sizeDate = g.MeasureString( m_sDate, f );
            if( m_bConserveWidth )
            {
                m_sizeFirstnames = g.MeasureString( m_sFirstnames, f );
                m_sizeSurname = g.MeasureString( m_sSurname, f );
            }
            else
            {
                m_sizeFirstnames.Height = 0;
                m_sizeFirstnames.Width = 0;
                m_sizeSurname = g.MeasureString( Name, f );
            }

            if( m_sizeFirstnames.Width > m_sizeSurname.Width )
            {
                if( m_sizeDate.Width > m_sizeFirstnames.Width )
                {
                    m_sizeText.Width = m_sizeDate.Width;
                    m_fDatePad = 0f;
                    m_fFirstnamesPad = (m_sizeDate.Width - m_sizeFirstnames.Width )/2f;
                    m_fSurnamePad = (m_sizeDate.Width - m_sizeSurname.Width )/2f;
                }
                else
                {
                    m_sizeText.Width = m_sizeFirstnames.Width;
                    m_fDatePad = (m_sizeFirstnames.Width - m_sizeDate.Width )/2f;
                    m_fFirstnamesPad = 0f;
                    m_fSurnamePad = (m_sizeFirstnames.Width - m_sizeSurname.Width )/2f;
                }
            }
            else
            {
                if( m_sizeDate.Width > m_sizeSurname.Width )
                {
                    m_sizeText.Width = m_sizeDate.Width;
                    m_fDatePad = 0f;
                    m_fFirstnamesPad = (m_sizeDate.Width - m_sizeFirstnames.Width )/2f;
                    m_fSurnamePad = (m_sizeDate.Width - m_sizeSurname.Width )/2f;
                }
                else
                {
                    m_sizeText.Width = m_sizeSurname.Width;
                    m_fDatePad = (m_sizeSurname.Width - m_sizeDate.Width )/2f;
                    m_fFirstnamesPad = (m_sizeSurname.Width - m_sizeFirstnames.Width )/2f;
                    m_fSurnamePad = 0f;
                }
            }
            m_sizeText.Height = m_sizeFirstnames.Height + m_sizeSurname.Height + m_sizeDate.Height;

            m_sizeText.Width += PADDING_HORIZ*2f;
            m_sizeText.Height += PADDING_VERT*2f;
            
            m_size = m_sizeText;
            m_size.Width += MARGIN_HORIZ*2f;
            m_size.Height += MARGIN_VERT*2f;

            return m_size;
        }

        // Draws the actual box, and adds the region of the box to the image alMap list.
        public override void DrawBitmap( CPaintbox paintbox, Graphics g, ArrayList alImageMap )
        {
            SolidBrush solidbrushBg, solidbrushText;

            if( m_bConcealed )
            {
                solidbrushBg = paintbox.m_brushBoxConcealed;
            }
            else if( m_bHighlight )
            {
                solidbrushBg = paintbox.m_brushBoxHighlight;
            }
            else if( m_bShade )
            {
                solidbrushBg = paintbox.m_brushBoxShade;
            }
            else
            {
                solidbrushBg = paintbox.m_brushBox;
            }

            if( m_bLinkable )
            {
                solidbrushText = paintbox.m_brushTextLink;
            }
            else if( m_bConcealed )
            {
                solidbrushText = paintbox.m_brushTextConcealed;
            }
            else
            {
                solidbrushText = paintbox.m_brushText;
            }

            g.FillRectangle( solidbrushBg, m_x+MARGIN_HORIZ, m_y+MARGIN_VERT, m_sizeText.Width, m_sizeText.Height-1f );
            g.DrawRectangle( paintbox.m_penBox, m_x+MARGIN_HORIZ, m_y+MARGIN_VERT, m_sizeText.Width, m_sizeText.Height-1f );

            float fTextX = m_x+MARGIN_HORIZ+PADDING_HORIZ;
            float fTextY = m_y+MARGIN_VERT+PADDING_VERT;
            if( m_bConserveWidth )
            {
                g.DrawString( m_sFirstnames, paintbox.m_font, solidbrushText, fTextX + m_fFirstnamesPad, fTextY );
                fTextY += m_sizeFirstnames.Height;
                g.DrawString( m_sSurname, paintbox.m_font, solidbrushText, fTextX + m_fSurnamePad, fTextY );
                fTextY += m_sizeSurname.Height;
            }
            else
            {
                g.DrawString( Name, paintbox.m_font, solidbrushText, fTextX + m_fSurnamePad, fTextY );
                fTextY += m_sizeSurname.Height;
            }

            g.DrawString( m_sDate, paintbox.m_font, solidbrushText, fTextX + m_fDatePad, fTextY );
            
            if( m_bChild )
            {
                g.DrawLine( paintbox.m_penConnector, m_x+MARGIN_HORIZ+m_sizeText.Width/2f, m_y, m_x+MARGIN_HORIZ+m_sizeText.Width/2f, m_y + MARGIN_VERT/* -1f*/ );
            }

            if( m_ir != null )
            {
                alImageMap.Add( new CMiniTreeMap( Name, m_ir, m_bLinkable,
                    (int)(m_x+MARGIN_HORIZ), (int)(m_y+MARGIN_VERT), 
                    (int)(m_x+MARGIN_HORIZ+m_sizeText.Width), (int)(m_y+MARGIN_VERT+m_sizeText.Height-1f) ) );
            }
        }

        // Accessor for the individual's name
        private string Name
        {
            get
            {
                return String.Concat( m_sFirstnames, " ", m_sSurname );
            }
        }

        // Sets the location of this box. Size should have already been
        // calculated by now.
        public override SizeF CalculateLayout( float x, float y )
        {
            m_x = x;
            m_y = y;

            return m_size;
        }

        // Accessor for left side position of box sArea.
        public float Left
        {
            get
            {
                return m_x;
            }
        }

        // Accessor for right side position of box sArea.
        public float Right
        {
            get
            {
                return m_x + m_size.Width;
            }
        }

        // Accessor for top side position of box sArea.
        public float Top
        {
            get
            {
                return m_y;
            }
        }

        // Accessor for bottom side position of box sArea.
        public float Bottom
        {
            get
            {
                return m_y + m_size.Height;
            }
        }

        // The vertical centre of the box, where the tee gedcomLine between boxes should attach.
        public float TeeCentreVert
        {
            get
            {
                return m_y + MARGIN_VERT + m_sizeText.Height/2f;
            }
        }

        // The left edge of the box, where the tee gedcomLine between boxes should attach.
        public float TeeLeft
        {
            get
            {
                return m_x + MARGIN_HORIZ;
            }
        }

        // The right edge of the box, where the tee gedcomLine between boxes should attach.
        public float TeeRight
        {
            get
            {
                return m_x + m_size.Width - MARGIN_HORIZ;
            }
        }

        // Caller should call HasStalk first to ensure nResult from this property is valid.
        // The horizontal centre of the box, where the tee gedcomLine up to frParents should attach.
        public float Stalk
        {
            get
            {
                return m_x + m_size.Width/2f;
            }
        }

        // Returns true if this box needs a gedcomLine drawing up to a parent.
        public bool HasStalk
        {
            get
            {
                return m_bChild;
            }
        }

        // Moves this object left and all objects to its right left, until one of them
        // can't move without breaking the diagram.
        // Returns amount actually moved, which may be less than amount asked for.
        public override float PullLeft( float fAmount )
        {
            if( RightObject != null )
            {
                if( RightObject is CMiniTreeGroup )
                {
                    // Groups are stretchy so we can ignore if they get 
                    // stuck( and hence reduce 'amount')
                    RightObject.PullLeft( fAmount );
                }
                else
                {
                    fAmount = RightObject.PullLeft( fAmount );
                }
            }
            if( fAmount <= 0f )
            {
                // Nothing to do
                return fAmount;
            }

            // Individuals are free to move unless anchored by an attached group.
            m_x -= fAmount; 

            return fAmount;
        }

        // Moves this object right and all objects to its left right, until one of them
        // can't move without breaking the diagram.
        // Returns amount actually moved, which may be less than amount asked for.
        public override float PullRight( float fAmount )
        {
            if( LeftObject != null )
            {
                if( LeftObject is CMiniTreeGroup )
                {
                    // Groups are stretchy so we can ignore if they 
                    // get stuck( and hence reduce 'amount')
                    LeftObject.PullRight( fAmount );
                }
                else
                {
                    fAmount = LeftObject.PullRight( fAmount );
                }
            }
            if( fAmount <= 0f )
            {
                // Nothing to do
                return fAmount;
            }
            // Individuals are free to move unless anchored by an attached group.
            m_x += fAmount; 
            
            return fAmount;
        }

        // Moves this object left and all objects to its left left, until one of them
        // can't move without breaking the diagram.
        // Returns amount actually moved, which may be less than amount asked for.
        public override float PushLeft(float fAmount)
        {
            if( LeftObject != null )
            {
                fAmount = LeftObject.PushLeft( fAmount );
            } 
            else if( LeftObjectAlien != null )
            {
                if( LeftObjectAlien is CMiniTreeIndividual )
                {
                    // Push left until hit alien object
                    float distance = Left - ((CMiniTreeIndividual)LeftObjectAlien).Right;
                    if( distance < fAmount )
                    {
                        fAmount = distance;
                    }
                }
            }
            if( fAmount <= 0f )
            {
                // Nothing to do
                return fAmount;
            }
            // Individuals are free to move unless anchored by an attached group.
            m_x -= fAmount; 

            return fAmount;
        }

        // Moves this object right and all objects to its right right, until one of them
        // can't move without breaking the diagram.
        // Returns amount actually moved, which may be less than amount asked for.
        public override float PushRight(float fAmount)
        {
            if( RightObject != null )
            {
                fAmount = RightObject.PushRight( fAmount );
            }
            else if( RightObjectAlien != null )
            {
                if( RightObjectAlien is CMiniTreeIndividual )
                {
                    // Push right until hit alien object
                    float distance = ((CMiniTreeIndividual)RightObjectAlien).Left - Right;
                    if( distance < fAmount )
                    {
                        fAmount = distance;
                    }
                }
            }

            if( fAmount <= 0f )
            {
                // Nothing to do
                return fAmount;
            }
            m_x += fAmount; // Individuals are free to move unless anchored by a group attached

            return fAmount;
        }

        // Moves the position of the box by an absolute amount.
        public override void Translate( float fDeltaX, float fDeltaY )
        {
            m_x += fDeltaX;
            m_y += fDeltaY;
        }
    }
}

