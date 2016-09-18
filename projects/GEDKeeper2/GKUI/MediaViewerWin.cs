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
using System.IO;
using System.Text;
using System.Windows.Forms;

using GKCommon.Controls;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKUI
{
    /// <summary>
    /// 
    /// </summary>
    public partial class MediaViewerWin : Form, ILocalization
    {
        private readonly IBaseWindow fBase;

        private GEDCOMFileReferenceWithTitle fFileRef;
        private ImageView fImageCtl;

        public GEDCOMFileReferenceWithTitle FileRef
        {
            get { return this.fFileRef; }
            set { this.SetFileRef(value); }
        }

        private void SetFileRef(GEDCOMFileReferenceWithTitle value)
        {
            this.fFileRef = value;
            this.Text = this.fFileRef.Title;
            Control ctl = null;

            MultimediaKind mmKind = GKUtils.GetMultimediaKind(this.fFileRef.MultimediaFormat);

            try
            {
                switch (mmKind)
                {
                    case MultimediaKind.mkImage:
                        {
                            Image img = this.fBase.Context.LoadMediaImage(this.fFileRef, false);
                            this.fImageCtl = new ImageView();
                            this.fImageCtl.OpenImage(img);
                            ctl = this.fImageCtl;
                            break;
                        }

                    case MultimediaKind.mkAudio:
                    case MultimediaKind.mkVideo:
                        // error
                        break;

                    case MultimediaKind.mkText:
                        {
                            Stream fs;
                            this.fBase.Context.MediaLoad(this.fFileRef, out fs, false);

                            switch (this.fFileRef.MultimediaFormat)
                            {
                                case GEDCOMMultimediaFormat.mfTXT:
                                    using (StreamReader strd = new StreamReader(fs, Encoding.GetEncoding(1251))) {
                                        TextBox txtBox = new TextBox();
                                        txtBox.Multiline = true;
                                        txtBox.ReadOnly = true;
                                        txtBox.ScrollBars = ScrollBars.Both;
                                        txtBox.Text = strd.ReadToEnd();
                                        ctl = txtBox;
                                    }
                                    break;

                                case GEDCOMMultimediaFormat.mfRTF:
                                    using (StreamReader strd = new StreamReader(fs)) {
                                        RichTextBox rtfBox = new RichTextBox();
                                        rtfBox.ReadOnly = true;
                                        rtfBox.Text = strd.ReadToEnd();
                                        ctl = rtfBox;
                                    }
                                    break;

                                case GEDCOMMultimediaFormat.mfHTM:
                                    ctl = new WebBrowser();
                                    (ctl as WebBrowser).DocumentStream = fs;
                                    break;
                            }
                            break;
                        }
                }

                if (ctl != null) {
                    this.SuspendLayout();

                    ctl.Dock = DockStyle.Fill;
                    ctl.Location = new Point(0, 0);
                    ctl.Size = new Size(100, 100);
                    base.Controls.Add(ctl);
                    base.Controls.SetChildIndex(ctl, 0);

                    this.ResumeLayout(false);
                }
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("MediaViewerWin.SetFileRef(): " + ex.Message);

                if (ctl != null) ctl.Dispose();
            }
        }

        private void MediaViewerWin_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Escape) {
                base.Close();
            }
        }

        public MediaViewerWin(IBaseWindow aBase)
        {
            this.InitializeComponent();
            this.fBase = aBase;

            this.SetLang();
        }

        public void SetLang()
        {
            if (this.fImageCtl != null) {
                this.fImageCtl.btnSizeToFit.Text = LangMan.LS(LSID.LSID_SizeToFit);
                this.fImageCtl.btnZoomIn.Text = LangMan.LS(LSID.LSID_ZoomIn);
                this.fImageCtl.btnZoomOut.Text = LangMan.LS(LSID.LSID_ZoomOut);
            }
        }
    }
}
