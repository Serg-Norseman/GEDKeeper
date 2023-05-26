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

namespace GEDmill.MiniTree
{
    /// <summary>
    /// Base class for elements in the tree diagram.
    /// </summary>
    public abstract class MTObject
    {
        protected readonly ITreeDrawer fDrawer;

        // The object to the left of this one.
        protected MTObject fLeft;

        // The object to the right of this one.
        protected MTObject fRight;

        protected MTObject fLeftAlien;

        protected MTObject fRightAlien;


        // Tells this object about the one to its left.
        public MTObject LeftObject
        {
            get { return fLeft; }
            set { fLeft = value; }
        }

        // Tells this object about the one to its right.
        public MTObject RightObject
        {
            get { return fRight; }
            set { fRight = value; }
        }

        public MTObject LeftObjectAlien
        {
            get { return fLeftAlien; }
            set { fLeftAlien = value; }
        }

        public MTObject RightObjectAlien
        {
            get { return fRightAlien; }
            set { fRightAlien = value; }
        }


        protected MTObject(ITreeDrawer drawer)
        {
            fDrawer = drawer;
            fLeft = null;
            fRight = null;
            fLeftAlien = null;
            fRightAlien = null;
        }

        // Calculates the size required by the element.
        public abstract ExtSizeF CalculateSize();

        // Calculates how to lay out this element.
        public abstract ExtSizeF CalculateLayout(float x, float y);

        // Shifts this object and all objects to its left, until one can't move.
        public abstract float PullLeft(float amount);

        // Shifts this object and all objects to its right, until one can't move.
        public abstract float PullRight(float amount);

        // Pushes this object left and all objects to its left left, until one can't move.
        public abstract float PushLeft(float amount);

        // Shifts this object and all objects to its right, until one can't move.
        public abstract float PushRight(float amount);

        // Moves the position of this object by an absolute amount.
        public abstract void Translate(float deltaX, float deltaY);
    }
}
