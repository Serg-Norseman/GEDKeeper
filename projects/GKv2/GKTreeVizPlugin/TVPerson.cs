/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Collections.Generic;
using System.Drawing;
using GDModel;

namespace GKTreeVizPlugin
{
    internal enum TVPersonType
    {
        Patriarch,
        Spouse,
        Child
    }

    internal class TVPerson
    {
        private static int NextIdx = 1;
        private static readonly Random random = new Random();
        
        public readonly int Idx;

        public TVPerson Parent;
        public int BirthYear, DeathYear;
        public int DescGenerations;
        public TVPersonType Type;

        public readonly GDMIndividualRecord IRec;
        public readonly GDMSex Sex;
        public readonly List<TVPerson> Spouses;
        public readonly List<TVPerson> Childs;

        public readonly int BeautySpouses;
        public readonly int BeautyChilds;

        public float BaseRadius;
        public float GenSlice;
        public bool IsVisible;
        public PointF Pt;
        
        public TVStem Stem;

        public TVPerson(TVPerson parent, GDMIndividualRecord iRec)
        {
            Idx = NextIdx++;

            Parent = parent;
            IRec = iRec;
            Sex = iRec.Sex;

            Spouses = new List<TVPerson>();
            Childs = new List<TVPerson>();
            BeautySpouses = random.Next(0, 360);
            BeautyChilds = random.Next(0, 360);
        }
    }

    internal class TVStem
    {
        private static readonly Random random = new Random();

        public readonly List<TVPerson> Spouses;
        public readonly List<TVPerson> Childs;

        public readonly int BeautySpouses;
        public readonly int BeautyChilds;

        public PointF Pt;
        public float BaseRadius;
        public float GenSlice;
        //public bool IsFixedIntersection;

        public TVStem()
        {
            Spouses = new List<TVPerson>();
            Childs = new List<TVPerson>();

            BeautySpouses = random.Next(0, 360);
            BeautyChilds = random.Next(0, 360);
        }

        public void AddSpouse(TVPerson spouse)
        {
            // first item
            if (Spouses.Count == 0)
            {
                BaseRadius = spouse.BaseRadius;
                GenSlice = spouse.GenSlice;
                Pt = spouse.Pt;
            }

            spouse.Stem = this;
            Spouses.Add(spouse);
        }

        public void AddChild(TVPerson child)
        {
            child.Stem = this;
            Childs.Add(child);
        }

        public void Update()
        {
            // to recalculate the coordinates of the spouses, because the coordinates of this person could change
            // "genSlice / 3" - it's radius of spouses
            PointF[] pts = TreeVizControl.GetCirclePoints(BeautySpouses, Pt, Spouses.Count, GenSlice / 3);
            for (int i = 0, count = Spouses.Count; i < count; i++)
            {
                TVPerson spp = Spouses[i];
                spp.Pt = pts[i];
            }

            // recalculate coordinates of visible children
            pts = TreeVizControl.GetCirclePoints(BeautyChilds, Pt, Childs.Count, BaseRadius / 2);
            for (int i = 0, count = Childs.Count; i < count; i++)
            {
                TVPerson chp = Childs[i];
                chp.Pt = pts[i];
            }
        }
    }
}
