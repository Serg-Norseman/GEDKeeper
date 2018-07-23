/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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

using System.Collections.Generic;
using Eto.Drawing;
using Eto.Forms;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public partial class SlideshowWin : Form, IWorkWindow
    {
        private readonly IBaseWindow fBase;
        private readonly List<GEDCOMFileReferenceWithTitle> fFileRefs;
        private readonly ImageBox fImageCtl;

        private int fCurrentIndex;
        private string fCurrentText;
        private ITimer fTimer;

        public SlideshowWin(IBaseWindow baseWin)
        {
            InitializeComponent();

            SuspendLayout();
            fImageCtl = new GKUI.Components.ImageBox();
            Content = fImageCtl;
            ResumeLayout();

            fImageCtl.BackgroundColor = SystemColors.ControlBackground; //ControlDark;
            fImageCtl.ImageBorderStyle = ImageBoxBorderStyle.FixedSingleGlowShadow;
            fImageCtl.ImageBorderColor = Colors.AliceBlue;
            fImageCtl.SelectionMode = ImageBoxSelectionMode.Zoom;

            fTimer = AppHost.Instance.CreateTimer(1000, Timer1Tick);

            WindowState = Eto.Forms.WindowState.Maximized;

            SetLang();

            fBase = baseWin;
            fFileRefs = new List<GEDCOMFileReferenceWithTitle>();
            fCurrentIndex = -1;

            LoadList();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (fTimer != null) fTimer.Dispose();
            }
            base.Dispose(disposing);
        }

        private void SlideshowWin_Load(object sender, System.EventArgs e)
        {
            if (fFileRefs.Count > 0) {
                fCurrentIndex = 0;
                SetFileRef();
            } else {
                UpdateControls();
            }
        }

        private void SlideshowWin_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.Key == Keys.Escape) Close();
        }

        private void LoadList()
        {
            GEDCOMRecord record;
            var enumerator = fBase.Context.Tree.GetEnumerator(GEDCOMRecordType.rtMultimedia);
            while (enumerator.MoveNext(out record)) {
                GEDCOMMultimediaRecord mediaRec = (GEDCOMMultimediaRecord)record;
                GEDCOMFileReferenceWithTitle fileRef = mediaRec.FileReferences[0];

                MultimediaKind mmKind = GKUtils.GetMultimediaKind(fileRef.MultimediaFormat);
                if (mmKind == MultimediaKind.mkImage) {
                    fFileRefs.Add(fileRef);
                }
            }
        }

        public void SetLang()
        {
            Title = LangMan.LS(LSID.LSID_Slideshow);
            tbStart.Text = LangMan.LS(LSID.LSID_Start);
            tbPrev.ToolTip = LangMan.LS(LSID.LSID_PrevRec);
            tbNext.ToolTip = LangMan.LS(LSID.LSID_NextRec);
        }

        private void SetFileRef()
        {
            if (fCurrentIndex < 0 || fCurrentIndex >= fFileRefs.Count) return;

            // Only images are in the list
            GEDCOMFileReferenceWithTitle fileRef = fFileRefs[fCurrentIndex];

            fCurrentText = fileRef.Title;

            IImage img = fBase.Context.LoadMediaImage(fileRef, false);
            if (img != null) {
                fImageCtl.Image = ((ImageHandler)img).Handle;
                fImageCtl.ZoomToFit();
            }

            UpdateControls();
        }

        private void tsbStart_Click(object sender, System.EventArgs e)
        {
            if (tbStart.Text == LangMan.LS(LSID.LSID_Start)) {
                tbStart.Text = LangMan.LS(LSID.LSID_Stop);
                tbStart.Image = UIHelper.LoadResourceImage("Resources.btn_stop.gif");
                fTimer.Start();
            } else {
                tbStart.Text = LangMan.LS(LSID.LSID_Start);
                tbStart.Image = UIHelper.LoadResourceImage("Resources.btn_start.gif");
                fTimer.Stop();
            }
        }

        private void tsbPrev_Click(object sender, System.EventArgs e)
        {
            fCurrentIndex--;
            SetFileRef();
        }

        private void tsbNext_Click(object sender, System.EventArgs e)
        {
            fCurrentIndex++;
            SetFileRef();
        }

        private void UpdateControls()
        {
            tbStart.Enabled = (fFileRefs.Count > 0);
            tbPrev.Enabled = (fCurrentIndex > 0);
            tbNext.Enabled = (fCurrentIndex < fFileRefs.Count - 1);

            AppHost.Instance.UpdateControls(false);
        }

        private void Timer1Tick(object sender, System.EventArgs e)
        {
            if (fCurrentIndex < fFileRefs.Count - 1) {
                fCurrentIndex++;
            } else {
                fCurrentIndex = 0;
            }

            SetFileRef();
        }

        #region IWorkWindow implementation

        public string GetStatusString()
        {
            return string.Format("{0} / {1} [{2}]", fCurrentIndex + 1, fFileRefs.Count, fCurrentText);
        }

        public void UpdateView()
        {
            // dummy
        }

        public void NavNext()
        {
        }

        public void NavPrev()
        {
        }

        public bool NavCanBackward()
        {
            return false;
        }

        public bool NavCanForward()
        {
            return false;
        }

        public bool AllowQuickSearch()
        {
            return false;
        }

        public IList<ISearchResult> FindAll(string searchPattern)
        {
            return new List<ISearchResult>();
        }

        public void SelectByRec(GEDCOMIndividualRecord iRec)
        {
        }

        public void QuickSearch()
        {
        }

        public bool AllowFilter()
        {
            return false;
        }

        public void SetFilter()
        {
        }

        #endregion
    }
}
