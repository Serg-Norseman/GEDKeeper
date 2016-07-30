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
using GKCore.Interfaces;
using GKCore.Options;

namespace GKUI.Charts
{
    public class AncestorsCircle : CircleChart
    {
        private class PersonSegment : BaseObject
        {
            public readonly int Gen;
            public readonly GraphicsPath Path;

            public GEDCOMIndividualRecord IRec;
            public int Rad;
            public float V;
            public int GroupIndex;
            public PersonSegment FatherSegment;
            public PersonSegment MotherSegment;
            public float StartAngle;
            public float WedgeAngle;

            public PersonSegment(int generation)
            {
                this.Gen = generation;
                this.Path = new GraphicsPath();
                this.IRec = null;
                this.GroupIndex = -1;
                this.FatherSegment = null;
                this.MotherSegment = null;
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
        private int fGroupCount;
        private bool fGroupsMode;
        private string fHint;
        private int fIndividualsCount;
        private int fMaxGenerations;
        private int fOffsetX = 0;
        private int fOffsetY = 0;
        private GEDCOMIndividualRecord fRootPerson;
        private PersonSegment fSelected;
        //private GEDCOMTree fTree;

        private bool ArcText = true;

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

        public AncestorsCircle(IBaseWindow baseWin) : base()
        {
            this.components = new System.ComponentModel.Container();

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

        private void TraverseGroups(PersonSegment segment, int groupIndex)
        {
            if (segment == null) return;

            segment.GroupIndex = groupIndex;
            if (segment.FatherSegment != null) this.TraverseGroups(segment.FatherSegment, groupIndex);
            if (segment.MotherSegment != null) this.TraverseGroups(segment.MotherSegment, groupIndex);
        }

        private PersonSegment SetSegmentParams(int index, GEDCOMIndividualRecord rec, int rad, float v, int groupIndex)
        {
            try
            {
                PersonSegment segment = this.fSegments[index];

                segment.IRec = rec;
                segment.Rad = rad;
                segment.V = v;
                segment.GroupIndex = groupIndex;

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
                PersonSegment rootSegment = this.SetSegmentParams(0, this.fRootPerson, 0, 0, -1);

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

        private PersonSegment TraverseAncestors(GEDCOMIndividualRecord iRec, float v, int gen, int rad, float ro, int prevSteps, int groupIndex)
        {
            try
            {
                this.fIndividualsCount++;

                if (this.fGroupsMode && groupIndex == -1) {
                    PersonSegment otherSegment = this.FindSegmentByRec(iRec);
                    if (otherSegment != null) {
                        this.fGroupCount++;
                        groupIndex = this.fGroupCount;
                        this.TraverseGroups(otherSegment, groupIndex);
                    }
                }

                int genSize = (int)Math.Pow(2.0, gen);
                float ang = (360.0f / genSize);

                int idx = prevSteps + (int)(v / ang);
                PersonSegment segment = this.SetSegmentParams(idx, iRec, rad, v, groupIndex);

                if (gen < this.fMaxGenerations)
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

        #endregion

        #region Drawing

        protected override void InternalDraw(Graphics gfx)
        {
            gfx.SmoothingMode = SmoothingMode.AntiAlias;

            Pen pen = new Pen(this.Options.BrushColor[10]);

            int num = this.fSegments.Count;
            for (int i = 0; i < num; i++) {
                PersonSegment segment = this.fSegments[i];

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
                    gfx.DrawPath(pen, path);
                }
            }

            for (int i = 0; i < num; i++) {
                this.DrawAncestorName(gfx, this.fSegments[i]);
            }
        }

        private void DrawAncestorName(Graphics gfx, PersonSegment segment)
        {
            int rad = segment.Rad;
            int gen = segment.Gen;
            float v = segment.V;
            GEDCOMIndividualRecord iRec = segment.IRec;

            string surn, givn;
            if (iRec == null) {
                if (gen == 0) {
                    givn = "Choose";
                    surn = "subject";
                } else {
                    return;
                }
            } else {
                string dummy;
                iRec.GetNameParts(out surn, out givn, out dummy);
            }

            rad -= 20;
            switch (gen)
            {
                case 0:
                    {
                        gfx.ResetTransform();
                        gfx.TranslateTransform(this.fCenterX, this.fCenterY);

                        SizeF sizeF = gfx.MeasureString(surn, this.Font);
                        gfx.DrawString(surn, this.Font, this.fCircleBrushes[8], -sizeF.Width / 2f, -sizeF.Height / 2f - sizeF.Height / 2f);
                        sizeF = gfx.MeasureString(givn, this.Font);
                        gfx.DrawString(givn, this.Font, this.fCircleBrushes[8], -sizeF.Width / 2f, 0f);
                        break;
                    }

                case 1:
                    if (ArcText) {
                        SizeF sizeF = gfx.MeasureString(surn, this.Font);
                        DrawArcText(gfx, surn, this.fCenterX, this.fCenterY, rad + sizeF.Height / 2f,
                                    segment.StartAngle, segment.WedgeAngle, true, true, Font, this.fCircleBrushes[8]);

                        sizeF = gfx.MeasureString(givn, this.Font);
                        DrawArcText(gfx, givn, this.fCenterX, this.fCenterY, rad - sizeF.Height / 2f,
                                    segment.StartAngle, segment.WedgeAngle, true, true, Font, this.fCircleBrushes[8]);
                    } else {
                        float dx = (float)Math.Sin(PI * v / 180.0) * rad;
                        float dy = (float)Math.Cos(PI * v / 180.0) * rad;

                        gfx.ResetTransform();
                        gfx.TranslateTransform(this.fCenterX + dx, this.fCenterY - dy);
                        gfx.RotateTransform(v);

                        SizeF sizeF = gfx.MeasureString(surn, this.Font);
                        gfx.DrawString(surn, this.Font, this.fCircleBrushes[8], -sizeF.Width / 2f, -sizeF.Height / 2f);
                        sizeF = gfx.MeasureString(givn, this.Font);
                        gfx.DrawString(givn, this.Font, this.fCircleBrushes[8], -sizeF.Width / 2f, -sizeF.Height / 2f + sizeF.Height);
                    }
                    break;

                case 2:
                case 3:
                case 4:
                    if (ArcText) {
                        if (gen == 2) {
                            SizeF sizeF = gfx.MeasureString(surn, this.Font);
                            DrawArcText(gfx, surn, this.fCenterX, this.fCenterY, rad + sizeF.Height / 2f,
                                        segment.StartAngle, segment.WedgeAngle, true, true, Font, this.fCircleBrushes[8]);

                            sizeF = gfx.MeasureString(givn, this.Font);
                            DrawArcText(gfx, givn, this.fCenterX, this.fCenterY, rad - sizeF.Height / 2f,
                                        segment.StartAngle, segment.WedgeAngle, true, true, Font, this.fCircleBrushes[8]);
                        } else {
                            DrawArcText(gfx, givn, this.fCenterX, this.fCenterY, rad,
                                        segment.StartAngle, segment.WedgeAngle, true, true, Font, this.fCircleBrushes[8]);
                        }
                    } else {
                        float dx = (float)Math.Sin(PI * v / 180.0) * rad;
                        float dy = (float)Math.Cos(PI * v / 180.0) * rad;
                        gfx.ResetTransform();
                        gfx.TranslateTransform(this.fCenterX + dx, this.fCenterY - dy);
                        gfx.RotateTransform(v);

                        SizeF sizeF2 = gfx.MeasureString(surn, this.Font);
                        gfx.DrawString(surn, this.Font, this.fCircleBrushes[8], -sizeF2.Width / 2f, -sizeF2.Height / 2f);
                        sizeF2 = gfx.MeasureString(givn, this.Font);
                        dx = (float)Math.Sin(PI * v / 180.0) * (rad - sizeF2.Height);
                        dy = (float)Math.Cos(PI * v / 180.0) * (rad - sizeF2.Height);
                        gfx.ResetTransform();
                        gfx.TranslateTransform(this.fCenterX + dx, this.fCenterY - dy);
                        gfx.RotateTransform(v);
                        gfx.DrawString(givn, this.Font, this.fCircleBrushes[8], -sizeF2.Width / 2f, -sizeF2.Height / 2f);
                    }
                    break;

                case 5:
                    {
                        float dx = (float)Math.Sin(PI * v / 180.0) * rad;
                        float dy = (float)Math.Cos(PI * v / 180.0) * rad;
                        gfx.ResetTransform();
                        gfx.TranslateTransform(this.fCenterX + dx, this.fCenterY - dy);
                        gfx.RotateTransform(v);

                        this.DrawString(gfx, givn);
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

                        this.DrawString(gfx, givn);
                        break;
                    }
            }
        }

        private void DrawString(Graphics gfx, string str)
        {
            SizeF size = gfx.MeasureString(str, this.Font);
            gfx.DrawString(str, this.Font, this.fCircleBrushes[8], -size.Width / 2f, -size.Height / 2f);
        }

        private static void DrawArcText(Graphics graphics, string text, float centerX, float centerY, float radius,
                                        float startAngle, float wedgeAngle,
                                        bool inside, bool clockwise, Font font, Brush brush)
        {
            var size = graphics.MeasureString(text, font);
            radius = radius + size.Height / 2.0f;

            float textAngle = (float)GfxHelper.RadiansToDegrees((size.Width * 1.75f) / radius);
            float deltaAngle = (wedgeAngle - textAngle) / 2.0f;

            startAngle -= 90.0f + deltaAngle;

            for (var i = 0; i < text.Length; ++i)
            {
                graphics.ResetTransform();

                float offset = (textAngle * ((i + 0.0f) / text.Length));
                offset = clockwise ? -offset : offset;
                float angle = startAngle + offset;

                double radAngle = angle * (Math.PI / 180.0d);
                float x = (float)(centerX + Math.Sin(radAngle) * radius);
                float y = (float)(centerY + Math.Cos(radAngle) * radius);
                graphics.TranslateTransform(x, y);

                double charRotation = inside ? -angle + 180 : -angle;
                graphics.RotateTransform((float)(charRotation));

                string chr = new String(text[i], 1);
                graphics.DrawString(chr, font, brush, 0, 0);
            }
        }

        #endregion

        #region Protected methods

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
                    {
                        if (this.fRootPerson != null) {
                            GEDCOMIndividualRecord father, mother;
                            this.fRootPerson.GetParents(out father, out mother);

                            if (father != null) {
                                this.RootPerson = father;
                            }
                        }
                        break;
                    }

                case Keys.Right:
                    {
                        if (this.fRootPerson != null) {
                            GEDCOMIndividualRecord father, mother;
                            this.fRootPerson.GetParents(out father, out mother);

                            if (mother != null) {
                                this.RootPerson = mother;
                            }
                        }
                        break;
                    }

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
