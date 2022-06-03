/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using System.Windows.Forms;
using GDModel;
using GKCore;
using GKCore.Interfaces;
using GKUI.Components;
using GKUI.Forms;

namespace GKImageViewerPlugin
{
    /// <summary>
    /// 
    /// </summary>
    public partial class ImageViewerWin : CommonWindow
    {
        private ImageView fImageCtl;
        private readonly Plugin fPlugin;


        public ImageViewerWin(Plugin plugin)
        {
            InitializeComponent();

            tbFileLoad.Image = UIHelper.LoadResourceImage("Resources.btn_load.gif");

            fPlugin = plugin;

            SetLocale();
        }

        private void ImageViewerWin_FormClosed(object sender, FormClosedEventArgs e)
        {
            fPlugin.fForm = null;
        }

        private void ImageViewerWin_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Escape) {
                Close();
            }
        }

        private void ToolBar1_ButtonClick(object sender, EventArgs e)
        {
            if (sender == tbFileLoad) {
                string fileName = AppHost.StdDialogs.GetOpenFile("", "", fPlugin.LangMan.LS(PLS.LSID_FilesFilter), 1, "");
                if (!string.IsNullOrEmpty(fileName)) {
                    SetFileRef(fileName);
                }
            }
        }

        private void SetFileRef(string fileName)
        {
            Text = fileName;
            Control ctl = null;
            fImageCtl = null;

            GDMMultimediaFormat fmt = GDMFileReference.RecognizeFormat(fileName);

            try {
                switch (fmt) {
                    case GDMMultimediaFormat.mfBMP:
                    case GDMMultimediaFormat.mfGIF:
                    case GDMMultimediaFormat.mfJPG:
                    case GDMMultimediaFormat.mfPCX:
                    case GDMMultimediaFormat.mfTIF:
                    case GDMMultimediaFormat.mfTGA:
                    case GDMMultimediaFormat.mfPNG:
                        {
                            fImageCtl = new ImageView();

                            try {
                                using (Stream fs = new FileStream(fileName, FileMode.Open, FileAccess.Read)) {
                                    fImageCtl.OpenImage(new Bitmap(fs));
                                }
                            } catch (Exception ex) {
                                Logger.WriteError("ImageViewerWin.SetFileRef.0()", ex);
                            }

                            ctl = fImageCtl;
                        }
                        break;

                    case GDMMultimediaFormat.mfWAV:
                    case GDMMultimediaFormat.mfAVI:
                    case GDMMultimediaFormat.mfMPG:
                        break;

                    case GDMMultimediaFormat.mfTXT:
                        {
                            TextBox txtBox = new TextBox();
                            txtBox.Multiline = true;
                            txtBox.ReadOnly = true;
                            txtBox.ScrollBars = ScrollBars.Both;

                            try {
                                using (Stream fs = new FileStream(fileName, FileMode.Open, FileAccess.Read)) {
                                    using (StreamReader strd = GKUtils.GetDetectedStreamReader(fs)) {
                                        txtBox.Text = strd.ReadToEnd();
                                    }
                                }
                            } catch (Exception ex) {
                                Logger.WriteError("ImageViewerWin.SetFileRef.1()", ex);
                            }

                            ctl = txtBox;
                        }
                        break;

                    case GDMMultimediaFormat.mfRTF:
                        {
                            RichTextBox rtfBox = new RichTextBox();
                            rtfBox.ReadOnly = true;

                            try {
                                using (Stream fs = new FileStream(fileName, FileMode.Open, FileAccess.Read)) {
                                    using (StreamReader strd = new StreamReader(fs)) {
                                        rtfBox.Text = strd.ReadToEnd();
                                    }
                                }
                            } catch (Exception ex) {
                                Logger.WriteError("ImageViewerWin.SetFileRef.2()", ex);
                            }

                            ctl = rtfBox;
                        }
                        break;

                    case GDMMultimediaFormat.mfHTM:
                        {
                            var browser = new WebBrowser();
                            try {
                                using (Stream fs = new FileStream(fileName, FileMode.Open, FileAccess.Read)) {
                                    browser.DocumentStream = fs;
                                }
                            } catch (Exception ex) {
                                Logger.WriteError("ImageViewerWin.SetFileRef.2()", ex);
                            }
                            ctl = browser;
                        }
                        break;
                }

                if (ctl != null) {
                    SuspendLayout();

                    ctl.Dock = DockStyle.Fill;
                    ctl.Location = new Point(0, 50);
                    ctl.Size = new Size(100, 100);
                    Controls.Add(ctl);
                    Controls.SetChildIndex(ctl, 0);

                    ResumeLayout(false);
                    PerformLayout();
                }
            } catch (Exception ex) {
                if (ctl != null) ctl.Dispose();

                Logger.WriteError("ImageViewerWin.SetFileRef()", ex);
            }
        }

        #region ILocalizable support

        public override void SetLocale()
        {
            Text = fPlugin.LangMan.LS(PLS.LSID_ImgViewer);
            tbFileLoad.ToolTipText = fPlugin.LangMan.LS(PLS.LSID_FileLoad);

            if (fImageCtl != null) {
                fImageCtl.SetLocale();
            }
        }

        #endregion
    }
}
