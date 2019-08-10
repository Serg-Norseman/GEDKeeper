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

using GDModel;

namespace GEDmill.MiniTree
{
    /// <summary>
    /// A data structure containing coordinates of a clickable sArea on the mini tree.
    /// Used to create HTML image alMap.
    /// </summary>
    public class MiniTreeMap
    {
        // The name of the individual in the box. Used for ALT attribute.
        public string Name;

        // The left coordinate of the box.
        public int X1;

        // The top coordinate of the box.
        public int Y1;

        // The right coordinate of the box.
        public int X2;

        // The bottom coordinate of the box.
        public int Y2;

        // The individual record reached by clicking this box. Used for HREF.
        public GDMIndividualRecord IndiRec;

        // Whether this individual should be a clickable region on the alMap.
        public bool Linkable;


        public MiniTreeMap(string name, GDMIndividualRecord ir, bool linkable, int x1, int y1, int x2, int y2)
        {
            X1 = x1;
            Y1 = y1;
            X2 = x2;
            Y2 = y2;
            IndiRec = ir;
            Linkable = linkable;
            Name = name;
        }
    }
}
