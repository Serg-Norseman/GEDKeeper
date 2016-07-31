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
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Windows.Forms;

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;

namespace GKUI.Charts
{
    public class DescendantsCircle : CircleChart
    {
        private class PersonSegment : CircleSegment
        {
            public List<PersonSegment> ChildSegments;
            public int TotalSubSegments;

            public PersonSegment(int generation) : base(generation)
            {
                this.ChildSegments = new List<DescendantsCircle.PersonSegment>();
                this.TotalSubSegments = 0;
            }
        }


        public DescendantsCircle(IBaseWindow baseWin) : base(baseWin)
        {
        }

        protected override void BuildPathTree()
        {
            this.fSegments.Clear();

            // traverse tree
            this.fIndividualsCount = 0;

            PersonSegment rootSegment;
            if (this.fRootPerson != null) {
                rootSegment = this.TraverseDescendants(this.fRootPerson, 0);
            } else {
                return;
            }

            this.fCenterX = base.Width / 2 + this.fOffsetX;
            this.fCenterY = base.Height / 2 + this.fOffsetY;

            int inRad = CENTER_RAD - 50;
            float stepAngle = (360.0f / rootSegment.TotalSubSegments);

            this.CalcDescendants(rootSegment, inRad, -90.0f, stepAngle);
        }

        private void CalcDescendants(PersonSegment segment, int inRad, float startAngle, float stepAngle)
        {
            GraphicsPath path = segment.Path;

            int ctX = this.fCenterX;
            int ctY = this.fCenterY;

            int extRad;
            if (segment.Gen == 0) {
                segment.WedgeAngle = 360.0f;

                path.StartFigure();
                path.AddEllipse(ctX - inRad, ctY - inRad, inRad * 2, inRad * 2);
                path.CloseFigure();

                extRad = inRad;
            } else {
                extRad = inRad + this.fGenWidth;

                int size = (segment.TotalSubSegments > 0) ? segment.TotalSubSegments : 1;
                float wedgeAngle = stepAngle * size;

                int ir2 = inRad * 2;
                int er2 = extRad * 2;

                float ang1 = startAngle;
                float angval1 = ang1 * PI / 180.0f;
                int px1 = ctX + (int)(inRad * Math.Cos(angval1));
                int py1 = ctY + (int)(inRad * Math.Sin(angval1));
                int px2 = ctX + (int)(extRad * Math.Cos(angval1));
                int py2 = ctY + (int)(extRad * Math.Sin(angval1));

                float ang2 = ang1 + wedgeAngle;
                float angval2 = ang2 * PI / 180.0f;
                int nx1 = ctX + (int)(inRad * Math.Cos(angval2));
                int ny1 = ctY + (int)(inRad * Math.Sin(angval2));
                int nx2 = ctX + (int)(extRad * Math.Cos(angval2));
                int ny2 = ctY + (int)(extRad * Math.Sin(angval2));

                segment.StartAngle = startAngle;
                segment.WedgeAngle = wedgeAngle;
                segment.Rad = inRad + 50;

                path.StartFigure();
                path.AddLine(px2, py2, px1, py1);
                path.AddArc(ctX - inRad, ctY - inRad, ir2, ir2, ang1, wedgeAngle);
                path.AddLine(nx1, ny1, nx2, ny2);
                path.AddArc(ctX - extRad, ctY - extRad, er2, er2, ang2, -wedgeAngle);
                path.CloseFigure();
            }

            float childStartAngle = startAngle;
            for (int i = 0; i < segment.ChildSegments.Count; i++) {
                PersonSegment childSegment = segment.ChildSegments[i];

                this.CalcDescendants(childSegment, extRad, childStartAngle, stepAngle);

                int steps = (childSegment.TotalSubSegments > 0) ? childSegment.TotalSubSegments : 1;

                childStartAngle += stepAngle * steps;
            }
        }

        private PersonSegment TraverseDescendants(GEDCOMIndividualRecord iRec, int gen)
        {
            if (iRec == null) return null;
            
            try
            {
                this.fIndividualsCount++;

                PersonSegment resultSegment = new PersonSegment(gen);
                resultSegment.IRec = iRec;
                this.fSegments.Add(resultSegment);

                if (gen < this.fMaxGenerations)
                {
                    int num2 = iRec.SpouseToFamilyLinks.Count;
                    for (int j = 0; j < num2; j++)
                    {
                        GEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[j].Family;
                        if (GKUtils.IsRecordAccess(family.Restriction, this.fShieldState))
                        {
                            family.SortChilds();

                            int num3 = family.Childrens.Count;
                            for (int i = 0; i < num3; i++)
                            {
                                GEDCOMIndividualRecord child = family.Childrens[i].Value as GEDCOMIndividualRecord;
                                PersonSegment childSegment = this.TraverseDescendants(child, gen + 1);

                                int size = (childSegment.TotalSubSegments > 0) ? childSegment.TotalSubSegments : 1;
                                resultSegment.TotalSubSegments += size;

                                resultSegment.ChildSegments.Add(childSegment);
                            }
                        }
                    }
                }

                return resultSegment;
            }
            catch
            {
                return null;
            }
        }

        protected override void InternalDraw(Graphics gfx)
        {
            gfx.SmoothingMode = SmoothingMode.AntiAlias;

            Pen pen = new Pen(this.Options.BrushColor[10]);

            int num = this.fSegments.Count;
            for (int i = 0; i < num; i++) {
                PersonSegment segment = (PersonSegment)this.fSegments[i];

                if (segment.IRec != null) {
                    int brIndex;
                    brIndex = (segment.Gen == 0) ? 9 : segment.Gen - 1;

                    SolidBrush brush;
                    brush = (this.fSelected == segment) ? this.fDarkBrushes[brIndex] : this.fCircleBrushes[brIndex];

                    GraphicsPath path = segment.Path;
                    gfx.FillPath(brush, path);
                    gfx.DrawPath(pen, path);
                }
            }

            for (int i = 0; i < num; i++) {
                this.DrawPersonName(gfx, this.fSegments[i]);
            }
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
