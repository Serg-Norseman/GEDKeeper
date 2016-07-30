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

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Options;
using GKCore.Types;

namespace GKUI.Charts
{
    public class DescendantsCircle : CircleChart
    {
        private class PersonSegment : BaseObject
        {
            public readonly int Gen;
            public readonly GraphicsPath Path;

            public GEDCOMIndividualRecord IRec;
            public int Rad;
            public float V;
            public List<PersonSegment> ChildSegments;
            public int TotalSubSegments;
            //public float StartAngle;
            //public float WedgeAngle;

            public PersonSegment(int generation)
            {
                this.Gen = generation;
                this.Path = new GraphicsPath();
                this.IRec = null;
                this.ChildSegments = new List<DescendantsCircle.PersonSegment>();
                this.TotalSubSegments = 0;
            }

            protected override void Dispose(bool disposing)
            {
                if (disposing)
                {
                    if (this.Path != null) this.Path.Dispose();
                }
                base.Dispose(disposing);
            }
        }

        private const float PI = 3.1415926535897931f;
        private const int CENTER_RAD = 90;
        private const int DEFAULT_GEN_WIDTH = 60;

        private readonly System.ComponentModel.IContainer components;
        private readonly SolidBrush[] fCircleBrushes;
        private readonly SolidBrush[] fDarkBrushes;
        private readonly AncestorsCircleOptions fOptions;
        private readonly List<PersonSegment> fSegments;
        private readonly ToolTip fToolTip;

        private int fCenterX;
        private int fCenterY;
        private int fGenWidth;
        private string fHint;
        private int fIndividualsCount;
        private int fMaxGenerations;
        private int fOffsetX = 0;
        private int fOffsetY = 0;
        private GEDCOMIndividualRecord fRootPerson;
        private PersonSegment fSelected;
        private ShieldState fShieldState;
        //private GEDCOMTree fTree;


        public AncestorsCircleOptions Options
        {
            get { return this.fOptions; }
        }

        public int IndividualsCount
        {
            get { return this.fIndividualsCount; }
        }

        public GEDCOMIndividualRecord RootPerson
        {
            get {
                return this.fRootPerson;
            }
            set {
                this.fRootPerson = value;

                this.NavAdd(value);
                this.Changed();

                this.DoRootChanged(value);
            }
        }

        public int GenWidth
        {
            get {
                return this.fGenWidth;
            }
            set {
                if (value < 20 || value > 100) return;
                
                this.fGenWidth = value;
                this.Changed();
            }
        }
        
        public DescendantsCircle(IBaseWindow baseWin) : base()
        {
            this.components = new System.ComponentModel.Container();

            this.fShieldState = baseWin.ShieldState;
            
            //this.fTree = tree;
            this.fOptions = new AncestorsCircleOptions();
            this.fCircleBrushes = new SolidBrush[AncestorsCircleOptions.MAX_BRUSHES];
            this.fDarkBrushes = new SolidBrush[AncestorsCircleOptions.MAX_BRUSHES];

            this.DoubleBuffered = true;
            this.BackColor = this.fOptions.BrushColor[9];
            this.fSegments = new List<PersonSegment>();
            this.fSelected = null;
            this.fGenWidth = DEFAULT_GEN_WIDTH;
            this.fMaxGenerations = 8;

            this.fToolTip = new ToolTip(components);
            this.fToolTip.AutoPopDelay = 5000;
            this.fToolTip.InitialDelay = 250;
            this.fToolTip.ReshowDelay = 50;
            this.fToolTip.ShowAlways = true;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (components != null) components.Dispose();
            }
            base.Dispose(disposing);
        }

        protected override void SetNavObject(object obj)
        {
            this.RootPerson = obj as GEDCOMIndividualRecord;
        }

        #region Content
        
        private void CreateBrushes()
        {
            for (int i = 0; i < this.fOptions.BrushColor.Length; i++)
            {
                Color col = this.fOptions.BrushColor[i];

                this.fCircleBrushes[i] = new SolidBrush(col);
                this.fDarkBrushes[i] = new SolidBrush(GfxHelper.Darker(col, 0.2f));
            }
        }

        private PersonSegment SetSegmentParams(int index, GEDCOMIndividualRecord rec, int rad, float v, int groupIndex)
        {
            try
            {
                PersonSegment segment = this.fSegments[index];

                segment.IRec = rec;
                segment.Rad = rad;
                segment.V = v;

                return segment;
            }
            catch
            {
                return null;
            }
        }

        private PersonSegment FindSegmentByRec(GEDCOMIndividualRecord iRec)
        {
            PersonSegment result = null;

            int num = this.fSegments.Count;
            for (int i = 0; i < num; i++) {
                PersonSegment segment = this.fSegments[i];

                if (segment.IRec == iRec) {
                    result = segment;
                    break;
                }
            }

            return result;
        }

        private PersonSegment FindSegment(int mX, int mY)
        {
            PersonSegment result = null;

            int num = this.fSegments.Count;
            for (int i = 0; i < num; i++) {
                PersonSegment segment = this.fSegments[i];

                if (segment.Path.IsVisible(mX, mY)) {
                    result = segment;
                    break;
                }
            }

            return result;
        }

        public override void Changed()
        {
            this.CreateBrushes();

            this.BuildPathTree();

            this.Invalidate();
        }

        private void BuildPathTree()
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

        private PersonSegment TraverseDescendants(GEDCOMIndividualRecord iRec, int gen/*, float v, int rad, float ro, int prevSteps*/)
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

                /*
                int genSize = (int)Math.Pow(2.0, gen);
                float ang = (360.0f / genSize);

                int idx = prevSteps + (int)(v / ang);
                PersonSegment segment = this.SetSegmentParams(idx, iRec, rad, v, groupIndex);

                if (gen < 8) {
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
                }*/

                return resultSegment;
            }
            catch
            {
                return null;
            }
        }

        #endregion

        #region Drawing

        protected override void InternalDraw(Graphics gfx)
        {
            gfx.SmoothingMode = SmoothingMode.AntiAlias;

            Pen pen = new Pen(this.Options.BrushColor[10]);

            int num = this.fSegments.Count;
            for (int i = 0; i < num; i++) {
                PersonSegment segment = this.fSegments[i];

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

            /*this.DrawAncestorName(gfx, 0, 0, 0, this.fRootPerson);

            for (int i = 0; i < num; i++) {
                PersonSegment segment = this.fSegments[i];
                this.DrawAncestorName(gfx, segment.Rad, segment.Gen, segment.V, segment.IRec);
            }*/
        }

        /*private void DrawAncestorName(Graphics gfx, int rad, int gen, float v, GEDCOMIndividualRecord iRec)
        {
            string s2, s1;
            if (iRec == null) {
                if (gen == 0) {
                    s1 = "Choose";
                    s2 = "subject";
                } else {
                    return;
                }
            } else {
                string dummy;
                iRec.GetNameParts(out s2, out s1, out dummy);
            }

            rad -= 20;
            switch (gen)
            {
                case 0:
                    {
                        gfx.ResetTransform();
                        gfx.TranslateTransform(this.fCenterX, this.fCenterY);

                        SizeF sizeF = gfx.MeasureString(s1, this.Font);
                        gfx.DrawString(s1, this.Font, this.fCircleBrushes[8], -sizeF.Width / 2f, -sizeF.Height / 2f - sizeF.Height / 2f);
                        sizeF = gfx.MeasureString(s2, this.Font);
                        gfx.DrawString(s2, this.Font, this.fCircleBrushes[8], -sizeF.Width / 2f, 0f);
                        break;
                    }
                case 1:
                    {
                        float dx = (float)Math.Sin(PI * v / 180.0) * rad;
                        float dy = (float)Math.Cos(PI * v / 180.0) * rad;
                        gfx.ResetTransform();
                        gfx.TranslateTransform(this.fCenterX + dx, this.fCenterY - dy);
                        gfx.RotateTransform(v);

                        SizeF sizeF = gfx.MeasureString(s1, this.Font);
                        gfx.DrawString(s1, this.Font, this.fCircleBrushes[8], -sizeF.Width / 2f, -sizeF.Height / 2f);
                        sizeF = gfx.MeasureString(s2, this.Font);
                        gfx.DrawString(s2, this.Font, this.fCircleBrushes[8], -sizeF.Width / 2f, -sizeF.Height / 2f + sizeF.Height);
                        break;
                    }
                case 2:
                case 3:
                case 4:
                case 5:
                    {
                        float dx = (float)Math.Sin(PI * v / 180.0) * rad;
                        float dy = (float)Math.Cos(PI * v / 180.0) * rad;
                        gfx.ResetTransform();
                        gfx.TranslateTransform(this.fCenterX + dx, this.fCenterY - dy);
                        gfx.RotateTransform(v);

                        SizeF sizeF2 = gfx.MeasureString(s1, this.Font);
                        gfx.DrawString(s1, this.Font, this.fCircleBrushes[8], -sizeF2.Width / 2f, -sizeF2.Height / 2f);
                        sizeF2 = gfx.MeasureString(s2, this.Font);
                        dx = (float)Math.Sin(PI * v / 180.0) * (rad - sizeF2.Height);
                        dy = (float)Math.Cos(PI * v / 180.0) * (rad - sizeF2.Height);
                        gfx.ResetTransform();
                        gfx.TranslateTransform(this.fCenterX + dx, this.fCenterY - dy);
                        gfx.RotateTransform(v);
                        gfx.DrawString(s2, this.Font, this.fCircleBrushes[8], -sizeF2.Width / 2f, -sizeF2.Height / 2f);
                        break;
                    }
                case 6:
                case 7:
                case 8:
                    {
                        float dx = (float)Math.Sin(PI * v / 180.0) * rad;
                        float dy = (float)Math.Cos(PI * v / 180.0) * rad;
                        gfx.ResetTransform();
                        gfx.TranslateTransform(this.fCenterX + dx, this.fCenterY - dy);
                        gfx.RotateTransform(v - 90.0f);

                        this.DrawString(gfx, s1);
                        break;
                    }
            }
        }*/

        private void DrawString(Graphics gfx, string str)
        {
            SizeF size = gfx.MeasureString(str, this.Font);
            gfx.DrawString(str, this.Font, this.fCircleBrushes[8], -size.Width / 2f, -size.Height / 2f);
        }

        #endregion

        #region Protected methods

        protected override void OnDoubleClick(EventArgs e)
        {
            base.OnDoubleClick(e);
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

            if (!e.Handled) this.Changed();
        }

        protected override void OnMouseUp(MouseEventArgs e)
        {
            base.OnMouseMove(e);

            PersonSegment selected = this.FindSegment(e.X, e.Y);

            if (selected != null && selected.IRec != null) {
                this.RootPerson = selected.IRec;
            }
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            base.OnMouseMove(e);

            PersonSegment selected = this.FindSegment(e.X, e.Y);

            string hint = "";
            if (!Equals(this.fSelected, selected)) {
                this.fSelected = selected;
                
                if (selected != null && selected.IRec != null) {
                    string name = selected.IRec.GetNameString(true, false);
                    hint = /*selected.Gen.ToString() + ", " + */name;
                }

                this.Invalidate();
            }

            if (!Equals(this.fHint, hint)) {
                this.fHint = hint;

                if (!string.IsNullOrEmpty(hint)) {
                    fToolTip.Show(hint, this, e.X, e.Y, 3000);
                }
            }
        }

        #endregion
    }
}
