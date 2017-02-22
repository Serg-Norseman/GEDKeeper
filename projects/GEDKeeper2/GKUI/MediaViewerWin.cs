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
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;

using GKCommon.Controls;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;
using GKMediaPlayer;

namespace GKUI
{
    /// <summary>
    /// 
    /// </summary>
    public partial class MediaViewerWin : Form, ILocalization
    {
        private readonly IBaseWindow fBase;
        private GEDCOMFileReferenceWithTitle fFileRef;
        private Control fViewer;

        public GEDCOMFileReferenceWithTitle FileRef
        {
            get { return fFileRef; }
            set { SetFileRef(value); }
        }

        private void SetFileRef(GEDCOMFileReferenceWithTitle value)
        {
            fFileRef = value;
            Text = fFileRef.Title;
            Control ctl = null;

            MultimediaKind mmKind = GKUtils.GetMultimediaKind(fFileRef.MultimediaFormat);

            try
            {
                switch (mmKind)
                {
                    case MultimediaKind.mkImage:
                        {
                            Image img = fBase.Context.LoadMediaImage(fFileRef, false);
                            SetViewImage(img);
                            break;
                        }

                    case MultimediaKind.mkAudio:
                    case MultimediaKind.mkVideo:
                        {
                            string targetFile = "";
                            fBase.Context.MediaLoad(fFileRef, ref targetFile);
                            SetViewMedia(targetFile);
                            break;
                        }

                    case MultimediaKind.mkText:
                        {
                            Stream fs;
                            fBase.Context.MediaLoad(fFileRef, out fs, false);

                            switch (fFileRef.MultimediaFormat)
                            {
                                case GEDCOMMultimediaFormat.mfTXT:
                                    // FIXME: fix encoding! and test other!!!
                                    using (StreamReader strd = new StreamReader(fs, Encoding.GetEncoding(1251))) {
                                        TextBox txtBox = new TextBox();
                                        txtBox.Multiline = true;
                                        txtBox.ReadOnly = true;
                                        txtBox.ScrollBars = ScrollBars.Both;
                                        txtBox.Text = strd.ReadToEnd();
                                        ctl = txtBox;
                                        SetViewControl(ctl);
                                    }
                                    break;

                                case GEDCOMMultimediaFormat.mfRTF:
                                    using (StreamReader strd = new StreamReader(fs)) {
                                        RichTextBox rtfBox = new RichTextBox();
                                        rtfBox.ReadOnly = true;
                                        rtfBox.Text = strd.ReadToEnd();
                                        ctl = rtfBox;
                                        SetViewControl(ctl);
                                    }
                                    break;

                                case GEDCOMMultimediaFormat.mfHTM:
                                    ctl = new WebBrowser();
                                    ((WebBrowser) ctl).DocumentStream = fs;
                                    SetViewControl(ctl);
                                    break;
                            }
                            break;
                        }
                }
            }
            catch (Exception ex)
            {
                if (ctl != null) ctl.Dispose();

                fBase.Host.LogWrite("MediaViewerWin.SetFileRef(): " + ex.Message);
            }
        }

        public void SetViewMedia(string mediaFile)
        {
            MediaPlayer mediaPlayer = new MediaPlayer();
            mediaPlayer.MediaFile = mediaFile;

            SetViewControl(mediaPlayer);
        }

        public void SetViewImage(Image img)
        {
            ImageView imageCtl = new ImageView();
            imageCtl.OpenImage(img);
            imageCtl.btnSizeToFit.Text = LangMan.LS(LSID.LSID_SizeToFit);
            imageCtl.btnZoomIn.Text = LangMan.LS(LSID.LSID_ZoomIn);
            imageCtl.btnZoomOut.Text = LangMan.LS(LSID.LSID_ZoomOut);

            SetViewControl(imageCtl);
        }

        private void SetViewControl(Control ctl)
        {
            if (ctl == null) return;
            fViewer = ctl;

            SuspendLayout();

            ctl.Dock = DockStyle.Fill;
            ctl.Location = new Point(0, 0);
            ctl.Size = new Size(100, 100);
            Controls.Add(ctl);
            Controls.SetChildIndex(ctl, 0);

            ResumeLayout(false);
        }

        public MediaViewerWin(IBaseWindow baseWin)
        {
            InitializeComponent();

            fBase = baseWin;
            SetLang();
        }

        public void SetLang()
        {
        }

        protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);
            if (fViewer != null) fViewer.Select();
        }

        private void MediaViewerWin_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Escape) {
                Close();
            }
        }

        private void MediaViewerWin_FormClosing(object sender, FormClosingEventArgs e)
        {
            if (fViewer is MediaPlayer) {
                ((MediaPlayer)fViewer).btnStop_Click(null, null);
            }
        }
    }
}
