/* CMiniTreeObject.cs
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

using System.Drawing;
using System.Collections;

namespace GEDmill.ImageClasses
{
    // Base class for elements in the tree diagram.
    public abstract class CMiniTreeObject
    {
        // The object to the left of this one.
        protected CMiniTreeObject m_mtoLeft;

        // The object to the right of this one.
        protected CMiniTreeObject m_mtoRight;
        
        protected CMiniTreeObject m_mtoLeftAlien;
        
        protected CMiniTreeObject m_mtoRightAlien;

        // Constructor.
        public CMiniTreeObject()
        {   
            m_mtoLeft = null;
            m_mtoRight = null;
            m_mtoLeftAlien = null;
            m_mtoRightAlien = null;
        }

        // Calculates the size required by the element.
        public abstract SizeF CalculateSize( Graphics g, Font f );

        // Draws the element to the graphics instance.
        public abstract void DrawBitmap( CPaintbox paintbox, Graphics g, ArrayList alMap );

        // Calculates how to lay out this element.
        public abstract SizeF CalculateLayout( float x, float y );

        // Shifts this object and all objects to its left, until one can't move.
        public abstract float PullLeft(float fAmount);

        // Shifts this object and all objects to its right, until one can't move.
        public abstract float PullRight(float fAmount);

        // Pushes this object left and all objects to its left left, until one can't move.
        public abstract float PushLeft(float fAmount);

        // Shifts this object and all objects to its right, until one can't move.
        public abstract float PushRight( float fAmount );

        // Moves the position of this object by an absolute amount.
        public abstract void Translate( float fDeltaX, float fDeltaY );

        // Tells this object about the one to its left.
        public CMiniTreeObject LeftObject
        {
            get
            {
                return m_mtoLeft;
            }
            set
            {
                m_mtoLeft = value;
            }
        }

        // Tells this object about the one to its right.
        public CMiniTreeObject RightObject
        {
            get
            {
                return m_mtoRight;
            }
            set
            {
                m_mtoRight = value;
            }
        }

        public CMiniTreeObject LeftObjectAlien
        {
            get
            {
                return m_mtoLeftAlien;
            }
            set
            {
                m_mtoLeftAlien = value;
            }
        }

        public CMiniTreeObject RightObjectAlien
        {
            get
            {
                return m_mtoRightAlien;
            }
            set
            {
                m_mtoRightAlien = value;
            }
        }

    }
}
