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
using GKCore.Options;

namespace GKUI.Charts
{
    public delegate void ARootChangedEventHandler(object sender, GEDCOMIndividualRecord person);

    public class AncestorsCircle : CustomChart
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

        private static readonly object EventRootChanged;

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
        
        public event ARootChangedEventHandler RootChanged
        {
            add { base.Events.AddHandler(AncestorsCircle.EventRootChanged, value); }
            remove { base.Events.RemoveHandler(AncestorsCircle.EventRootChanged, value); }
        }

        static AncestorsCircle()
        {
            AncestorsCircle.EventRootChanged = new object();
        }

        public AncestorsCircle(GEDCOMTree tree) : base()
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

        private void DoRootChanged(GEDCOMIndividualRecord person)
        {
            ARootChangedEventHandler eventHandler = (ARootChangedEventHandler)base.Events[AncestorsCircle.EventRootChanged];
            if (eventHandler == null) return;

            eventHandler(this, person);
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

        public void Changed()
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
                    path = segment.Path;
                    path.StartFigure();
                    path.AddLine(px2, py2, px1, py1);
                    path.AddArc(ctX - inRad, ctY - inRad, ir2, ir2, ang1, stepAngle);
                    path.AddLine(nx1, ny1, nx2, ny2);
                    path.AddArc(ctX - extRad, ctY - extRad, er2, er2, ang2, -stepAngle);
                    path.CloseFigure();
                    this.fSegments.Add(segment);
                }

                inRad = extRad;
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

        private void InternalDraw(Graphics gfx)
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

            /*if (!this.fGroupsMode)*/
            {
                this.DrawAncestorName(gfx, 0, 0, 0, this.fRootPerson);

                for (int i = 0; i < num; i++) {
                    PersonSegment segment = this.fSegments[i];
                    this.DrawAncestorName(gfx, segment.Rad, segment.Gen, segment.V, segment.IRec);
                }
            }
        }

        private void DrawAncestorName(Graphics gfx, int rad, int gen, float v, GEDCOMIndividualRecord iRec)
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
        }

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
            this.GroupsMode = !this.GroupsMode;
        }

        protected override bool IsInputKey(Keys keyData)
        {
            switch (keyData) {
                case Keys.Add:
                case Keys.Subtract:
                case Keys.Left:
                case Keys.Right:
                case Keys.Back:
                    return true;
            }

            return base.IsInputKey(keyData);
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

        protected override void OnPaint(PaintEventArgs e)
        {
            this.InternalDraw(e.Graphics);
        }

        protected override void OnResize(EventArgs e)
        {
            base.OnResize(e);
            this.Changed();
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
                    hint = selected.Gen.ToString() + ", " + name;
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
