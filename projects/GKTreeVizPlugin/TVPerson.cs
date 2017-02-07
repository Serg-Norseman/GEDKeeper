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

using GKCommon.GEDCOM;

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

		public readonly GEDCOMIndividualRecord IRec;
		public readonly GEDCOMSex Sex;
		public readonly List<TVPerson> Spouses;
		public readonly List<TVPerson> Childs;

		public readonly int BeautySpouses;
		public readonly int BeautyChilds;

		public float BaseRadius;
		public float GenSlice;
		public bool IsVisible;
		public PointF Pt;
		
		public TVStem Stem;

		public TVPerson(TVPerson parent, GEDCOMIndividualRecord iRec)
		{
			this.Idx = NextIdx++;

			this.Parent = parent;
			this.IRec = iRec;
			this.Sex = iRec.Sex;

			this.Spouses = new List<TVPerson>();
			this.Childs = new List<TVPerson>();
			this.BeautySpouses = random.Next(0, 360);
			this.BeautyChilds = random.Next(0, 360);
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
			this.Spouses = new List<TVPerson>();
			this.Childs = new List<TVPerson>();

			this.BeautySpouses = random.Next(0, 360);
			this.BeautyChilds = random.Next(0, 360);
		}

		public void addSpouse(TVPerson spouse)
		{
			// first item
			if (this.Spouses.Count == 0)
			{
				this.BaseRadius = spouse.BaseRadius;
				this.GenSlice = spouse.GenSlice;
				this.Pt = spouse.Pt;
			}

			spouse.Stem = this;
			this.Spouses.Add(spouse);
		}

		public void addChild(TVPerson child)
		{
			child.Stem = this;
			this.Childs.Add(child);
		}

		public void update()
		{
			// to recalculate the coordinates of the spouses, because the coordinates of this person could change
			// "genSlice / 3" - it's radius of spouses
			PointF[] pts = TreeVizControl.getCirclePoints(this.BeautySpouses, this.Pt, this.Spouses.Count, this.GenSlice / 3);
			for (int i = 0, count = this.Spouses.Count; i < count; i++)
			{
				TVPerson spp = this.Spouses[i];
				spp.Pt = pts[i];
			}

			// recalculate coordinates of visible children
			pts = TreeVizControl.getCirclePoints(this.BeautyChilds, this.Pt, this.Childs.Count, this.BaseRadius / 2);
			for (int i = 0, count = this.Childs.Count; i < count; i++)
			{
				TVPerson chp = this.Childs[i];
				chp.Pt = pts[i];
			}
		}
	}
}
