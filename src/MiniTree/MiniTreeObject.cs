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

using System.Collections;
using System.Drawing;

namespace GEDmill.MiniTree
{
    /// <summary>
    /// Base class for elements in the tree diagram.
    /// </summary>
    public abstract class MiniTreeObject
    {
        // The object to the left of this one.
        protected MiniTreeObject fLeft;

        // The object to the right of this one.
        protected MiniTreeObject fRight;

        protected MiniTreeObject fLeftAlien;

        protected MiniTreeObject fRightAlien;


        // Tells this object about the one to its left.
        public MiniTreeObject LeftObject
        {
            get { return fLeft; }
            set { fLeft = value; }
        }

        // Tells this object about the one to its right.
        public MiniTreeObject RightObject
        {
            get { return fRight; }
            set { fRight = value; }
        }

        public MiniTreeObject LeftObjectAlien
        {
            get { return fLeftAlien; }
            set { fLeftAlien = value; }
        }

        public MiniTreeObject RightObjectAlien
        {
            get { return fRightAlien; }
            set { fRightAlien = value; }
        }


        protected MiniTreeObject()
        {
            fLeft = null;
            fRight = null;
            fLeftAlien = null;
            fRightAlien = null;
        }

        // Calculates the size required by the element.
        public abstract SizeF CalculateSize(Graphics g, Font f);

        // Draws the element to the graphics instance.
        public abstract void DrawBitmap(Paintbox paintbox, Graphics g, ArrayList alMap);

        // Calculates how to lay out this element.
        public abstract SizeF CalculateLayout(float x, float y);

        // Shifts this object and all objects to its left, until one can't move.
        public abstract float PullLeft(float fAmount);

        // Shifts this object and all objects to its right, until one can't move.
        public abstract float PullRight(float fAmount);

        // Pushes this object left and all objects to its left left, until one can't move.
        public abstract float PushLeft(float fAmount);

        // Shifts this object and all objects to its right, until one can't move.
        public abstract float PushRight(float fAmount);

        // Moves the position of this object by an absolute amount.
        public abstract void Translate(float fDeltaX, float fDeltaY);
    }
}
