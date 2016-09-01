/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Windows.Forms;

using GKCommon.GEDCOM;
using GKCore.Interfaces;

namespace GKUI.Charts
{
    public class AncestorsCircle : CircleChart
    {
        private class PersonSegment : CircleSegment
        {
            public int GroupIndex;
            public PersonSegment FatherSegment;
            public PersonSegment MotherSegment;

            public PersonSegment(int generation) : base(generation)
            {
                this.GroupIndex = -1;
                this.FatherSegment = null;
                this.MotherSegment = null;
            }
        }


        private int fGroupCount;
        private bool fGroupsMode;


        public bool GroupsMode
        {
            get {
                return this.fGroupsMode;
            }
            set {
                this.fGroupsMode = value;
                if (value) {
                    this.fMaxGenerations = 8;
                    this.GenWidth = 60;
                } else {
                    this.fMaxGenerations = 8;
                    this.GenWidth = DEFAULT_GEN_WIDTH;
                }
            }
        }


        public AncestorsCircle(IBaseWindow baseWin) : base(baseWin)
        {
        }

        protected override void BuildPathTree()
        {
            this.fSegments.Clear();

            int ctX = base.Width / 2 + this.fOffsetX;
            int ctY = base.Height / 2 + this.fOffsetY;
            this.fCenterX = ctX;
            this.fCenterY = ctY;

            int inRad = CENTER_RAD - 50;

            PersonSegment segment = new PersonSegment(0);
            GraphicsPath path = segment.Path;
            path.StartFigure();
            path.AddEllipse(ctX - inRad, ctY - inRad, inRad * 2, inRad * 2);
            path.CloseFigure();
            this.fSegments.Add(segment);

            int maxSteps = 1;
            for (int gen = 1; gen <= this.fMaxGenerations; gen++) {
                inRad = (CENTER_RAD - 50) + ((gen - 1) * this.fGenWidth);

                int extRad = inRad + this.fGenWidth;
                maxSteps *= 2;

                int ir2 = inRad * 2;
                int er2 = extRad * 2;
                float stepAngle = (360.0f / maxSteps);

                for (int stp = 0; stp < maxSteps; stp++)
                {
                    float ang1 = (stp * stepAngle) - 90.0f;
                    float angval1 = ang1 * PI / 180.0f;
                    int px1 = ctX + (int)(inRad * Math.Cos(angval1));
                    int py1 = ctY + (int)(inRad * Math.Sin(angval1));
                    int px2 = ctX + (int)(extRad * Math.Cos(angval1));
                    int py2 = ctY + (int)(extRad * Math.Sin(angval1));

                    float ang2 = ang1 + stepAngle;
                    float angval2 = ang2 * PI / 180.0f;
                    int nx1 = ctX + (int)(inRad * Math.Cos(angval2));
                    int ny1 = ctY + (int)(inRad * Math.Sin(angval2));
                    int nx2 = ctX + (int)(extRad * Math.Cos(angval2));
                    int ny2 = ctY + (int)(extRad * Math.Sin(angval2));

                    segment = new PersonSegment(gen);
                    segment.StartAngle = ang1;
                    segment.WedgeAngle = stepAngle;

                    path = segment.Path;
                    path.StartFigure();
                    path.AddLine(px2, py2, px1, py1);
                    path.AddArc(ctX - inRad, ctY - inRad, ir2, ir2, ang1, stepAngle);
                    path.AddLine(nx1, ny1, nx2, ny2);
                    path.AddArc(ctX - extRad, ctY - extRad, er2, er2, ang2, -stepAngle);
                    path.CloseFigure();
                    this.fSegments.Add(segment);
                }
            }

            // traverse tree
            this.fGroupCount = -1;
            this.fIndividualsCount = 0;
            if (this.fRootPerson != null) {
                this.fIndividualsCount++;
                PersonSegment rootSegment = this.SetSegmentParams(0, this.fRootPerson, 0, -1);
                rootSegment.WedgeAngle = 360.0f;

                GEDCOMIndividualRecord father, mother;
                this.fRootPerson.GetParents(out father, out mother);

                if (mother != null) {
                    rootSegment.MotherSegment = this.TraverseAncestors(mother, 90f, 1, CENTER_RAD, 90.0f, 1, -1);
                }

                if (father != null) {
                    rootSegment.FatherSegment = this.TraverseAncestors(father, 270.0f, 1, CENTER_RAD, 90.0f, 1, -1);
                }
            }
        }

        private PersonSegment SetSegmentParams(int index, GEDCOMIndividualRecord rec, int rad, int groupIndex)
        {
            if (index < 0 || index >= this.fSegments.Count) {
                return null;
            } else {
                PersonSegment segment = (PersonSegment)this.fSegments[index];
                segment.IRec = rec;
                segment.Rad = rad;
                segment.GroupIndex = groupIndex;
                return segment;
            }
        }

        private PersonSegment TraverseAncestors(GEDCOMIndividualRecord iRec, float v, int gen, int rad, float ro, int prevSteps, int groupIndex)
        {
            try
            {
                this.fIndividualsCount++;

                if (this.fGroupsMode && groupIndex == -1) {
                    PersonSegment otherSegment = (PersonSegment)this.FindSegmentByRec(iRec);
                    if (otherSegment != null) {
                        this.fGroupCount++;
                        groupIndex = this.fGroupCount;
                        this.TraverseGroups(otherSegment, groupIndex);
                    }
                }

                int genSize = (int)Math.Pow(2.0, gen);
                float ang = (360.0f / genSize);

                int idx = prevSteps + (int)(v / ang);
                PersonSegment segment = this.SetSegmentParams(idx, iRec, rad, groupIndex);

                if (segment != null && gen < this.fMaxGenerations)
                {
                    GEDCOMIndividualRecord father, mother;
                    iRec.GetParents(out father, out mother);

                    int ps = prevSteps + genSize;

                    if (father != null) {
                        v -= (Math.Abs(ang - ro) / 2.0f);
                        segment.FatherSegment = this.TraverseAncestors(father, v, gen + 1, rad + this.fGenWidth, ro / 2.0f, ps, groupIndex);
                    }

                    if (mother != null) {
                        v += (ang / 2.0f);
                        segment.MotherSegment = this.TraverseAncestors(mother, v, gen + 1, rad + this.fGenWidth, ro / 2.0f, ps, groupIndex);
                    }
                }

                return segment;
            }
            catch
            {
                return null;
            }
        }

        private void TraverseGroups(PersonSegment segment, int groupIndex)
        {
            if (segment == null) return;

            segment.GroupIndex = groupIndex;
            if (segment.FatherSegment != null) this.TraverseGroups(segment.FatherSegment, groupIndex);
            if (segment.MotherSegment != null) this.TraverseGroups(segment.MotherSegment, groupIndex);
        }

        protected override void InternalDraw(Graphics gfx)
        {
            gfx.SmoothingMode = SmoothingMode.AntiAlias;

            int num = this.fSegments.Count;
            for (int i = 0; i < num; i++) {
                PersonSegment segment = (PersonSegment)this.fSegments[i];

                bool draw = (!this.Options.HideEmptySegments || segment.IRec != null);

                if (draw) {
                    int brIndex;
                    if (this.fGroupsMode) {
                        brIndex = (segment.GroupIndex == -1) ? 11 : segment.GroupIndex;
                    } else {
                        brIndex = (segment.Gen == 0) ? 9 : segment.Gen - 1;
                    }

                    SolidBrush brush;
                    brush = (this.fSelected == segment) ? this.fDarkBrushes[brIndex] : this.fCircleBrushes[brIndex];

                    GraphicsPath path = segment.Path;
                    gfx.FillPath(brush, path);
                    gfx.DrawPath(this.fPen, path);
                }
            }

            for (int i = 0; i < num; i++) {
                this.DrawPersonName(gfx, this.fSegments[i]);
            }
        }

        protected override void OnDoubleClick(EventArgs e)
        {
            base.OnDoubleClick(e);
            this.GroupsMode = !this.GroupsMode;
        }

        protected override void OnKeyDown(KeyEventArgs e)
        {
            base.OnKeyDown(e);

            e.Handled = false;
            switch (e.KeyCode) {
                case Keys.Add:
                    this.GenWidth += 10;
                    break;

                case Keys.Subtract:
                    this.GenWidth -= 10;
                    break;

                case Keys.Left:
                    if (this.fRootPerson != null) {
                        GEDCOMIndividualRecord father, mother;
                        this.fRootPerson.GetParents(out father, out mother);

                        if (father != null) {
                            this.RootPerson = father;
                        }
                    }
                    break;

                case Keys.Right:
                    if (this.fRootPerson != null) {
                        GEDCOMIndividualRecord father, mother;
                        this.fRootPerson.GetParents(out father, out mother);

                        if (mother != null) {
                            this.RootPerson = mother;
                        }
                    }
                    break;

                case Keys.Back:
                    this.NavPrev();
                    return;

                default:
                    e.Handled = true;
                    break;
            }
        }
    }
}
