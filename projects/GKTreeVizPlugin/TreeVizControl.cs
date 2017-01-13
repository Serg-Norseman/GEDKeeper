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
using System.Drawing.Imaging;
using System.Timers;
using System.Windows.Forms;

using ArborGVT;
using CsGL.OpenGL;
using GKCommon;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKTreeVizPlugin
{
    public sealed class TreeVizControl : OpenGLControl
    {
        private const float BASE_SCALE = 10.0f;
        private const bool EXCLUDE_CHILDLESS = true;	// exclude childless branches of the tree
        private const float MAGIC_SCALE = 4;			// scaling factor in the transition from the Arbor to the 3D
        private const float DEG2RAD = 3.14159F / 180;
        private const uint OBJ_X = 1;
        private const uint OBJ_Y = 2;
        private const uint OBJ_Z = 3;
        private const uint OBJ_NODE = 100;

        private const byte ACCUM_DEPTH = 0;         // OpenGL's Accumulation Buffer Depth, In Bits Per Pixel.
        private const byte STENCIL_DEPTH = 0;       // OpenGL's Stencil Buffer Depth, In Bits Per Pixel.
        private const byte Z_DEPTH = 16;            // OpenGL's Z-Buffer Depth, In Bits Per Pixel.
        private const byte COLOR_DEPTH = 16;        // The Current Color Depth, In Bits Per Pixel.
        private const double NEAR_CLIPPING_PLANE = 0.1f;    // GLU's Distance From The Viewer To The Near Clipping Plane (Always Positive).
        private const double FAR_CLIPPING_PLANE = 1000.0f;  // GLU's Distance From The Viewer To The Far Clipping Plane (Always Positive).
        private const double FOV_Y = 45.0f;         // GLU's Field Of View Angle, In Degrees, In The Y Direction.

        // unused
        //private float[] LightAmbient = {0.5f, 0.5f, 0.5f, 1.0f};
        //private float[] LightDiffuse = {1.0f, 1.0f, 1.0f, 1.0f};
        //private float[] LightPosition = {0.0f, 0.0f, 2.0f, 1.0f};
        //private int filter = 0;									// Which Filter To Use
        //private uint[] texture = new uint[3];						// Storage For 3 Textures

        // rendering
        private float xrot;
        private float yrot;
        private float zrot;
        private float z;

        // control
        private int fHeight;
        private int fWidth;
        private bool fMouseDrag;
        private int fLastX;
        private int fLastY;
        private bool fFreeRotate;

        // runtime
        private IBaseWindow fBase;
        private ArborSystem fSys;
        private readonly List<TVPerson> fPersons;
        private readonly Dictionary<string, TVPerson> fPersonsIndex;
        private readonly List<TVStem> fStems;
        private int fMaxYear;
        private int fMinYear;
        private float fYearSize;
        private bool fDebug;

        // animation
        private System.Timers.Timer fAnimTimer;
        private bool fBusy;
        private int fCurYear;
        private uint fTick;


        public int CurYear
        {
            get { return this.fCurYear; }
        }

        public bool Debug
        {
            get { return this.fDebug; }
            set { this.fDebug = value; }
        }

        public bool TimeStop { get; set; }
        public string SelectedObject { get; private set; }

        public bool FreeRotate
        {
            get {
                return this.fFreeRotate;
            }
            set {
                this.fFreeRotate = value;

                if (!value) {
                    this.xrot = -75;
                    this.yrot = 0.0F;
                }
            }
        }


        public TreeVizControl()
        {
            this.xrot = 0;
            this.yrot = 0;
            this.zrot = 0;
            this.z = -5.0f;
            this.Dock = DockStyle.Fill;

            GL.glShadeModel(GL.GL_SMOOTH);
            //GL.glClearColor(0.0f, 0.0f, 0.0f, 0.5f);
            GL.glClearColor(0.25f, 0.25f, 0.25f, 0.5f);
            GL.glClearDepth(1.0f);
            GL.glEnable(GL.GL_DEPTH_TEST);
            GL.glDepthFunc(GL.GL_LEQUAL);
            GL.glHint(GL.GL_PERSPECTIVE_CORRECTION_HINT, GL.GL_NICEST);

            //glEnable(GL_TEXTURE_2D);
            //LoadTextures();
            //GL.glLightfv(GL.GL_LIGHT1, GL.GL_AMBIENT, this.LightAmbient);
            //GL.glLightfv(GL.GL_LIGHT1, GL.GL_DIFFUSE, this.LightDiffuse);
            //GL.glLightfv(GL.GL_LIGHT1, GL.GL_POSITION, this.LightPosition);
            //GL.glEnable(GL.GL_LIGHT1);

            this.Context.Grab();
            OpenGLException.Assert();

            this.fPersons = new List<TVPerson>();
            this.fPersonsIndex = new Dictionary<string, TVPerson>();
            this.fStems = new List<TVStem>();

            this.fDebug = true;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fSys != null) fSys.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Control routines

        private uint retrieveObjectId(int x, int y)
        {
            /*int[] viewportCoords = {0, 0, 0, 0};
			uint[] selectBuffer = new uint[32];

			GL.glSelectBuffer(32, selectBuffer);
			GL.glGetIntegerv(GL.GL_VIEWPORT, viewportCoords);
			GL.glMatrixMode(GL.GL_PROJECTION);
			GL.glPushMatrix();
			GL.glRenderMode(GL.GL_SELECT);
			GL.glLoadIdentity();
			GL.gluPickMatrix(x, viewportCoords[3] - y, 2, 2, viewportCoords);
			GL.gluPerspective(FOV_Y, (float) fWidth / fHeight, NEAR_CLIPPING_PLANE, FAR_CLIPPING_PLANE);
			GL.glMatrixMode(GL.GL_MODELVIEW);

			this.glDraw();

            int objectsFound = GL.glRenderMode(GL.GL_RENDER);

			GL.glMatrixMode(GL.GL_PROJECTION);
			GL.glPopMatrix();
			GL.glMatrixMode(GL.GL_MODELVIEW);

			if (objectsFound > 0)
			{
				uint lowestDepth = selectBuffer[1];
				uint selectedObject = selectBuffer[3];

				for (int i = 1; i < objectsFound; i++)
				{
					if (selectBuffer[(i * 4) + 1] < lowestDepth)
					{
						lowestDepth = selectBuffer[(i * 4) + 1];
						selectedObject = selectBuffer[(i * 4) + 3];
					}
				}

				return selectedObject;
			}*/

            return 0;
        }

        protected override bool IsInputKey(Keys key)
        {
            switch(key) {
                case Keys.Up:
                case Keys.Down:
                case Keys.Right:
                case Keys.Left:
                case Keys.Tab:
                    return true;
                default:
                    return base.IsInputKey(key);
            }
        }

        protected override void OnKeyDown(KeyEventArgs e)
        {
            switch (e.KeyCode) {
                case Keys.F5:
                    this.screenshot();
                    break;

                case Keys.PageDown:
                    z -= 0.5f;
                    break;

                case Keys.PageUp:
                    z += 0.5f;
                    break;

                case Keys.R:
                    /*if (e.Control)*/ this.FreeRotate = !this.FreeRotate;
                    break;

                case Keys.D:
                    /*if (e.Control)*/ this.fDebug = !this.fDebug;
                    break;

                case Keys.T:
                    this.TimeStop = !this.TimeStop;
                    break;

                default:
                    base.OnKeyDown(e);
                    break;
            }
        }

        protected override void OnKeyUp(KeyEventArgs e)
        {
        }

        protected override void OnMouseDown(MouseEventArgs e)
        {
            base.OnMouseDown(e);

            if (!this.Focused) base.Focus();

            if (e.Button == MouseButtons.Left) {
                this.fMouseDrag = true;
                this.fLastX = e.X;
                this.fLastY = e.Y;
            }
        }

        protected override void OnMouseUp(MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Left) {
                this.fMouseDrag = false;
            }
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            base.OnMouseMove(e);

            if (this.fMouseDrag) {
                int dx = e.X - this.fLastX;
                int dy = e.Y - this.fLastY;

                if (this.fFreeRotate) {
                    //xrot += 0.001f * dy;
                    //yrot += 0.001f * dx;

                    xrot += 0.001f * dy; //ok
                    zrot += 0.001f * dx; //ok
                } else {
                    zrot += 0.001f * dx;
                }
            } else {
                uint objectId = this.retrieveObjectId(e.X, e.Y);

                switch (objectId) {
                    case OBJ_X:
                        SelectedObject = "X-axis";
                        break;
                    case OBJ_Y:
                        SelectedObject = "Y-axis";
                        break;
                    case OBJ_Z:
                        SelectedObject = "Z-axis";
                        break;
                    default:
                        if (objectId >= OBJ_NODE) {
                            int id = (int)(objectId - OBJ_NODE);

                            TVPerson prs = this.findPersonByIdx(id);
                            if (prs != null) {
                                SelectedObject = "["+prs.IRec.XRef+"] " + prs.IRec.GetPrimaryFullName()+
                                    ", " + prs.BirthYear.ToString() + " - " + prs.DeathYear.ToString();
                            } else {
                                SelectedObject = "<none>";
                            }
                        } else {
                            SelectedObject = "<none>";
                        }
                        break;
                }
            }
        }

        protected override void OnMouseWheel(MouseEventArgs e)
        {
            if (e.Delta != 0) {
                z += 0.01f * e.Delta;
            } else {
                base.OnMouseWheel(e);
            }
        }

        protected override void OnSizeChanged(EventArgs e)
        {
            Size sz = this.Size;

            if (sz.Width != 0 && sz.Height != 0)
            {
                this.fHeight = sz.Height;
                this.fWidth = sz.Width;

                if (this.fHeight == 0) {
                    this.fHeight = 1;
                }

                GL.glViewport(0, 0, this.fWidth, this.fHeight);
                GL.glMatrixMode(GL.GL_PROJECTION);
                GL.glLoadIdentity();

                GL.gluPerspective(FOV_Y, (float)this.fWidth / this.fHeight, NEAR_CLIPPING_PLANE, FAR_CLIPPING_PLANE);

                GL.glMatrixMode(GL.GL_MODELVIEW);
                GL.glLoadIdentity();
            }

            base.OnSizeChanged(e);
        }

        protected override OpenGLContext CreateContext()
        {
            ControlGLContext context = new ControlGLContext(this);
            DisplayType displayType = new DisplayType(COLOR_DEPTH, Z_DEPTH, STENCIL_DEPTH, ACCUM_DEPTH);
            context.Create(displayType, null);
            return context;
        }

        #endregion

        #region TreeViz

        public override void glDraw()
        {
            try
            {
                GL.glClear(GL.GL_COLOR_BUFFER_BIT | GL.GL_DEPTH_BUFFER_BIT);
                GL.glLoadIdentity();

                GL.glInitNames();

                GL.glTranslatef(0.0f, 0.0f, this.z);
                GL.glRotatef(this.xrot, 1.0f, 0.0f, 0.0f);
                GL.glRotatef(this.yrot, 0.0f, 1.0f, 0.0f);
                GL.glRotatef(this.zrot, 0.0f, 0.0f, 1.0f);

                drawAxis();

                // FIXME: labels of centuries are displayed from the beginning a personalized timeline,
                // need to alter the output according to chronological ages
                for (int i = 0; i <= 5; i++) {
                    GL.glPushMatrix();
                    GL.glTranslatef(0, 0, i * 100 * this.fYearSize);
                    GL.glColor3f(0.9F, 0.1F, 0.1F);
                    drawCircle(0.1F);
                    GL.glPopMatrix();
                }

                this.drawArborSystem();

                int num = fPersons.Count;
                for (int i = 0; i < num; i++)
                {
                    TVPerson prs = fPersons[i];
                    this.drawPerson(prs);
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("TreeVizControl.glDraw(): " + ex.Message);
            }
        }

        public void createArborGraph(IBaseWindow baseWin, int minGens, bool loneSuppress)
        {
            this.fBase = baseWin;

            try
            {
                fSys = new ArborSystem(1000, 1000, 0.1, null); //(10000, 1000, 0.1, this);
                fSys.setScreenSize(50, 50);
                fSys.OnStop += OnArborStop;

                using (ExtList<PatriarchObj> patList = baseWin.Context.GetPatriarchsLinks(minGens, false, loneSuppress))
                {
                    int num = patList.Count;
                    for (int i = 0; i < num; i++) {
                        PatriarchObj pObj = patList[i];

                        if ((!loneSuppress) || (loneSuppress && pObj.HasLinks)) {
                            ArborNode node = fSys.addNode(pObj.IRec.XRef);
                            node.Data = pObj;
                        }
                    }

                    for (int i = 0; i < num; i++)
                    {
                        PatriarchObj pat1 = patList[i];

                        foreach (PatriarchObj pat2 in pat1.Links)
                        {
                            fSys.addEdge(pat1.IRec.XRef, pat2.IRec.XRef);
                        }
                    }
                }

                this.z = -50;

                fSys.start();
            }
            catch (Exception ex)
            {
                Logger.LogWrite("TreeVizControl.createArborGraph(): " + ex.Message);
            }
        }

        public void OnArborStop(object sender, EventArgs eArgs)
        {
            this.FreeRotate = false;

            this.fMinYear = 0;

            try
            {
                // load from ArborSystem points and signatures of the patriarchs
                foreach (ArborNode node in fSys.Nodes)
                {
                    PatriarchObj patObj = (PatriarchObj)node.Data;

                    GEDCOMIndividualRecord iRec = (GEDCOMIndividualRecord)fBase.Tree.XRefIndex_Find(node.Sign);
                    int descGens = patObj.DescGenerations;

                    TVPerson patr = this.preparePerson(null, iRec, TVPersonType.Patriarch);
                    patr.Pt = new PointF((float)node.Pt.X * MAGIC_SCALE, (float)node.Pt.Y * MAGIC_SCALE);
                    patr.DescGenerations = descGens;
                    patr.BaseRadius = 100;

                    this.processPersonStem(patr, null, TVPersonType.Patriarch);

                    if (this.fMinYear == 0) {
                        this.fMinYear = patr.BirthYear;
                    } else {
                        if (this.fMinYear > patr.BirthYear) this.fMinYear = patr.BirthYear;
                    }
                }

                // prepare the radii of the bases of the patriarchs
                foreach (ArborEdge edge in fSys.Edges)
                {
                    TVPerson srcPers = this.findPersonByXRef(edge.Source.Sign);
                    TVPerson tgtPers = this.findPersonByXRef(edge.Target.Sign);

                    if (srcPers != null && tgtPers != null) {
                        float rad = (float)dist(srcPers.Pt, tgtPers.Pt) * 3/7;

                        if (srcPers.BaseRadius > rad) srcPers.BaseRadius = rad;
                        if (tgtPers.BaseRadius > rad) tgtPers.BaseRadius = rad;
                    }
                }

                // prepare the range of years
                this.fMaxYear = DateTime.Now.Year;
                this.fYearSize = BASE_SCALE / (this.fMaxYear - this.fMinYear);
                this.fTick = 0;
                this.fCurYear = this.fMinYear;

                // prepare tree, the base number - only the patriarchs
                int count = fPersons.Count;
                for (int i = 0; i < count; i++)
                {
                    TVPerson prs = fPersons[i];
                    this.prepareDescendants(prs);
                }

                for (int i = 0; i < this.fStems.Count; i++)
                {
                    TVStem stem = fStems[i];
                    stem.update();
                }

                this.startTimer();
            }
            catch (Exception ex)
            {
                Logger.LogWrite("TreeVizControl.onArborStop(): " + ex.Message);
            }
        }

        private void prepareDescendants(TVPerson person)
        {
            if (person == null) return;

            try
            {
                int gens = (person.DescGenerations <= 0) ? 1 : person.DescGenerations;
                person.GenSlice = person.BaseRadius / gens; // ?

                GEDCOMIndividualRecord iRec = person.IRec;

                foreach (GEDCOMSpouseToFamilyLink spLink in iRec.SpouseToFamilyLinks)
                {
                    GEDCOMFamilyRecord famRec = spLink.Family;

                    bool alreadyPrepared = false;

                    // processing the spouse of the current person
                    GEDCOMIndividualRecord spouse = famRec.GetSpouseBy(iRec);
                    if (spouse != null) {
                        TVPerson sps = this.preparePerson(null, spouse, TVPersonType.Spouse);
                        if (sps == null) {
                            // this can occur only when processing of the patriarchs later than those already processed,
                            // i.e. if the at first was already processed the patriarch, born in 1710, was processed his childrens
                            // and one of theirs spouses being a patriarch of new branches!
                            Logger.LogWrite("TreeVizControl.prepareDescendants(): an unexpected collision");
                            alreadyPrepared = true;
                        } else {
                            this.processPersonStem(sps, person, TVPersonType.Spouse);

                            person.Spouses.Add(sps);
                        }
                    }

                    if (!alreadyPrepared)
                    {
                        // processing children of the current family
                        foreach (GEDCOMPointer childPtr in famRec.Childrens)
                        {
                            GEDCOMIndividualRecord child = (GEDCOMIndividualRecord)childPtr.Value;

                            // exclude childless branches
                            if (EXCLUDE_CHILDLESS && (this.fBase.Context.IsChildless(child) || child.GetTotalChildsCount() < 1)) continue;

                            TVPerson chp = this.preparePerson(person, child, TVPersonType.Child);
                            if (chp == null) {
                                // this is someone spouse and already prepared, intersection of branches
                                Logger.LogWrite("TreeVizControl.prepareDescendants(): intersection");
                            } else {
                                chp.BaseRadius = (float)((person.BaseRadius / 2) * 0.95);
                                chp.DescGenerations = person.DescGenerations - 1;

                                this.processPersonStem(chp, person, TVPersonType.Child);

                                person.Childs.Add(chp);

                                this.prepareDescendants(chp);
                            }
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("TreeVizControl.prepareDescendants(): " + ex.Message);
            }
        }

        private void recalcDescendants(TVPerson person)
        {
            try
            {
                // recalculate the coordinates of the spouses, because the coordinates of this person could change
                // "genSlice / 3" - it's radius of spouses
                PointF[] pts = getCirclePoints(person.BeautySpouses, person.Pt, person.Spouses.Count, person.GenSlice / 3);
                for (int i = 0, count = person.Spouses.Count; i < count; i++)
                {
                    TVPerson spp = person.Spouses[i];
                    if (this.isVisible(spp)) {
                        spp.IsVisible = true;
                        person.Spouses[i].Pt = pts[i];
                    }
                }

                // recalculate of coordinates of visible children
                pts = getCirclePoints(person.BeautyChilds, person.Pt, person.Childs.Count, person.BaseRadius / 2);
                for (int i = 0, count = person.Childs.Count; i < count; i++)
                {
                    TVPerson chp = person.Childs[i];

                    if (!chp.IsVisible && this.isVisible(chp)) {
                        chp.IsVisible = true;
                    }

                    if (chp.IsVisible)
                    {
                        chp.Pt = pts[i];
                        chp.BaseRadius = (float)((person.BaseRadius / 2) * 0.95);
                        chp.DescGenerations = person.DescGenerations - 1;

                        this.recalcDescendants(chp);
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("TreeVizControl.recalcDescendants.2(): " + ex.Message);
            }
        }

        private void recalcDescendants()
        {
            try
            {
                foreach (TVPerson prs in fPersons)
                {
                    if (prs.Type == TVPersonType.Patriarch && this.isVisible(prs))
                    {
                        prs.IsVisible = true;
                        this.recalcDescendants(prs);
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("TreeVizControl.recalcDescendants.1(): " + ex.Message);
            }
        }

        private void processPersonStem(TVPerson person, TVPerson relative, TVPersonType type)
        {
            try
            {
                if (person == null) return;

                if (person.Stem == null && (type == TVPersonType.Patriarch || type == TVPersonType.Child))
                {
                    person.Stem = new TVStem();
                    this.fStems.Add(person.Stem);
                }

                switch (type)
                {
                    case TVPersonType.Spouse:
                        relative.Stem.addSpouse(person);
                        break;

                    case TVPersonType.Child:
                        relative.Stem.addChild(person);
                        break;

                    case TVPersonType.Patriarch:
                        break;
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("TreeVizControl.processPersonStem(): " + ex.Message);
            }
        }

        private TVPerson preparePerson(TVPerson parent, GEDCOMIndividualRecord iRec, TVPersonType type)
        {
            try
            {
                TVPerson result;

                if (this.fPersonsIndex.TryGetValue(iRec.XRef, out result))
                {
                    // the person is already in the general index; so this is someone's spouse in another tree previously processed

                    if (parent == null)
                    {
                        // parent == null, if this is patriarch or someone's spouse without his parents
                        // if it's a spouse and is already in the index - therefore,
                        // this person was married with the delegates of two different genera that are processed through their patriarchs?
                    }
                    else
                    {
                        result.Parent = parent;
                        PointF prevPt = result.Stem.Pt;
                        PointF parentPt = parent.Pt;
                        PointF midpoint = getLineMidpoint(prevPt.X, prevPt.Y, parentPt.X, parentPt.Y);
                        result.Stem.Pt = midpoint;
                        result.Stem.update();
                    }

                    return null;
                }
                else
                {
                    result = new TVPerson(parent, iRec);
                    result.Type = type;

                    this.fPersons.Add(result);
                    this.fPersonsIndex.Add(iRec.XRef, result);

                    result.BirthYear = fBase.Context.FindBirthYear(iRec);
                    result.DeathYear = fBase.Context.FindDeathYear(iRec);

                    // FIXME: alter to the pre-statistically a certain life expectancy
                    if (result.DeathYear == 0) result.DeathYear = result.BirthYear + 75;
                }

                return result;
            }
            catch (Exception ex)
            {
                Logger.LogWrite("TreeVizControl.preparePerson(): " + ex.Message);
                return null;
            }
        }

        private void drawArborSystem()
        {
            if (this.fSys == null) return;
            if (!this.fDebug) return;

            try
            {
                foreach (ArborNode node in fSys.Nodes)
                {
                    ArborPoint pt = node.Pt;

                    GL.glPushMatrix();
                    GL.glTranslatef((float)pt.X * MAGIC_SCALE, (float)pt.Y * MAGIC_SCALE, 0);
                    GL.glColor3f(0.9F, 0.3F, 0.3F);
                    drawCircle(0.1F);
                    GL.glPopMatrix();
                }

                foreach (ArborEdge edge in fSys.Edges)
                {
                    ArborPoint pt1 = edge.Source.Pt;
                    ArborPoint pt2 = edge.Target.Pt;

                    GL.glPushMatrix();
                    GL.glColor3f(0.9F, 0.3F, 0.3F);
                    GL.glBegin(GL.GL_LINES);
                    GL.glVertex3f((float)pt1.X * MAGIC_SCALE, (float)pt1.Y * MAGIC_SCALE, 0);
                    GL.glVertex3f((float)pt2.X * MAGIC_SCALE, (float)pt2.Y * MAGIC_SCALE, 0);
                    GL.glEnd();
                    GL.glPopMatrix();
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("TreeVizControl.drawArborSystem(): " + ex.Message);
            }
        }

        private bool isVisible(TVPerson person)
        {
            if (person == null) return false;
            if (person.BirthYear > this.fCurYear) return false;

            // not to display a person for which auto-detection of dates to no avail
            return (person.BirthYear >= this.fMinYear && person.DeathYear >= this.fMinYear);
        }

        private void drawPerson(TVPerson person)
        {
            if (person == null || !person.IsVisible) return;

            try
            {
                int endYear = (this.fCurYear < person.DeathYear) ? this.fCurYear : person.DeathYear;

                float zBirth, zDeath;
                zBirth = this.fYearSize * (person.BirthYear - this.fMinYear);
                zDeath = this.fYearSize * (endYear - this.fMinYear);

                PointF ppt = person.Pt;

                GL.glPushName(OBJ_NODE + (uint)person.Idx);
                GL.glPushMatrix();

                setLineColor(person.Sex);

                GL.glBegin(GL.GL_LINES);
                GL.glVertex3f(ppt.X, ppt.Y, zBirth);
                GL.glVertex3f(ppt.X, ppt.Y, zDeath);
                GL.glEnd();

                GL.glPopMatrix();
                GL.glPopName();

                if (this.fDebug && person.Type == TVPersonType.Patriarch) {
                    GL.glPushMatrix();
                    GL.glTranslatef(ppt.X, ppt.Y, 0);
                    GL.glColor3f(0.9F, 0.1F, 0.1F);
                    drawCircle(person.BaseRadius);
                    GL.glPopMatrix();
                }

                if (person.Parent != null) {
                    PointF parentPt = person.Parent.Pt;

                    GL.glPushMatrix();

                    setLineColor(person.Sex);

                    GL.glBegin(GL.GL_LINES);
                    GL.glVertex3f(parentPt.X, parentPt.Y, zBirth);
                    GL.glVertex3f(ppt.X, ppt.Y, zBirth);
                    GL.glEnd();

                    GL.glPopMatrix();
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("TreeVizControl.drawPerson(): " + ex.Message);
            }
        }

        private void updateTV(object sender, ElapsedEventArgs e)
        {
            if (this.fBusy) return;
            this.fBusy = true;
            try
            {
                if (!this.FreeRotate) {
                    this.zrot -= 0.3f;
                }

                this.fTick += 1;

                if (!this.TimeStop && (this.fTick % 5 == 0) && this.fCurYear < this.fMaxYear)
                {
                    this.fCurYear += 1;

                    this.recalcDescendants();
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("TreeVizControl.updateTV(): " + ex.Message);
            }
            this.fBusy = false;
        }

        #region Utils

        private TVPerson findPersonByXRef(string xref)
        {
            TVPerson result;
            return this.fPersonsIndex.TryGetValue(xref, out result) ? result : null;
        }

        private TVPerson findPersonByIdx(int idx)
        {
            foreach (TVPerson prs in this.fPersons)
            {
                if (prs.Idx == idx) return prs;
            }

            return null;
        }

        private void screenshot()
        {
            try {
                using (Image image = Context.ToImage()) {
                    image.Save(@"d:\screenshot.jpg", ImageFormat.Jpeg);
                }
            }
            catch (Exception e) {
                MessageBox.Show(e.Message, @"Error", MessageBoxButtons.OK, MessageBoxIcon.Stop);
            }
        }

        #endregion

        #region Low-level draw methods

        private static PointF getLineMidpoint(float x1, float y1, float x2, float y2)
        {
            float mx = x1 + (x2 - x1) / 2;
            float my = y1 + (y2 - y1) / 2;
            return new PointF(mx, my);
        }

        // beauty - random offset for "beauty", grad
        internal static PointF[] getCirclePoints(int beauty, PointF center, int count, float radius)
        {
            PointF[] result = new PointF[count];

            if (count > 0)
            {
                // size of the circle's partition, grad
                float degSection = 360.0f / count;

                for (int i = 0; i < count; i++)
                {
                    float degInRad = (beauty + i * degSection) * DEG2RAD;
                    float dx = (float)Math.Cos(degInRad) * radius;
                    float dy = (float)Math.Sin(degInRad) * radius;

                    result[i] = new PointF(center.X + dx, center.Y + dy);
                }
            }

            return result;
        }

        private static double dist(PointF pt1, PointF pt2)
        {
            double dx = pt2.X - pt1.X;
            double dy = pt2.Y - pt1.Y;
            return Math.Sqrt(dx * dx + dy * dy);
        }

        private static void setLineColor(GEDCOMSex sex)
        {
            switch (sex) {
                case GEDCOMSex.svMale:
                    GL.glColor3f(0.1F, 0.3F, 0.9F);
                    break;

                case GEDCOMSex.svFemale:
                    GL.glColor3f(0.9F, 0.3F, 0.1F);
                    break;
            }
        }

        private static void drawCircle(float radius)
        {
            GL.glBegin(GL.GL_LINE_LOOP);
            for (int i = 0; i <= 360; i++) {
                float degInRad = i * DEG2RAD;
                GL.glVertex2f((float)Math.Cos(degInRad) * radius, (float)Math.Sin(degInRad) * radius);
            }
            GL.glEnd();
        }

        private static void drawAxis()
        {
            // draw z-axis
            GL.glPushName(OBJ_Z);
            GL.glColor3f(1.0F, 1.0F, 1.0F);
            GL.glBegin(GL.GL_LINES); // z
            GL.glVertex3f(0, 0, 0);
            GL.glVertex3f(0, 0, 100);
            GL.glEnd();
            GL.glColor3f(0.5F, 0.5F, 0.5F);
            GL.glBegin(GL.GL_LINES); // z
            GL.glVertex3f(0, 0, 0);
            GL.glVertex3f(0, 0, -100);
            GL.glEnd();
            GL.glPopName();

            // draw y-axis
            GL.glPushName(OBJ_Y);
            GL.glColor3f(0.0F, 0.0F, 1.0F);
            GL.glBegin(GL.GL_LINES); // y
            GL.glVertex3f(0, 0, 0);
            GL.glVertex3f(0, 100, 0);
            GL.glEnd();
            GL.glColor3f(0.0F, 0.0F, 0.5F);
            GL.glBegin(GL.GL_LINES); // y
            GL.glVertex3f(0, 0, 0);
            GL.glVertex3f(0, -100, 0);
            GL.glEnd();
            GL.glPopName();

            // draw x-axis
            GL.glPushName(OBJ_X);
            GL.glColor3f(0.0F, 1.0F, 0.0F);
            GL.glBegin(GL.GL_LINES); // x
            GL.glVertex3f(0, 0, 0);
            GL.glVertex3f(100, 0, 0);
            GL.glEnd();
            GL.glColor3f(0.0F, 0.5F, 0.0F);
            GL.glBegin(GL.GL_LINES); // x
            GL.glVertex3f(0, 0, 0);
            GL.glVertex3f(-100, 0, 0);
            GL.glEnd();
            GL.glPopName();
        }

        #endregion

        #region Timer control

        private void startTimer()
        {
            if (fAnimTimer != null) return;

            fAnimTimer = new System.Timers.Timer();
            fAnimTimer.AutoReset = true;
            fAnimTimer.Interval = 20; //50;
            fAnimTimer.Elapsed += this.updateTV;
            fAnimTimer.Start();
        }

        private void stopTimer()
        {
            if (fAnimTimer == null) return;

            fAnimTimer.Stop();
            fAnimTimer = null;
        }

        #endregion

        #endregion
    }
}
