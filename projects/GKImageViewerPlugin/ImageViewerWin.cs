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

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKUI.Components;

namespace GKImageViewerPlugin
{
    /// <summary>
    /// 
    /// </summary>
    public partial class ImageViewerWin : Form, ILocalization, IMDIChild
    {
        private ImageView fImageCtl;
        private readonly Plugin fPlugin;

        public ImageViewerWin(Plugin plugin)
        {
            InitializeComponent();

            GKResourceManager resMgr = new GKResourceManager("IVPResource", typeof(ImageViewerWin).Assembly);
            tbFileLoad.Image = (Bitmap)resMgr.GetObjectEx("iLoad");

            fPlugin = plugin;

            SetLang();
        }

        private void ImageViewerWin_FormClosed(object sender, FormClosedEventArgs e)
        {
            fPlugin.fForm = null;
        }

        private void ImageViewerWin_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Escape)
            {
                Close();
            }
        }

        private void ToolBar1_ButtonClick(object sender, EventArgs e)
        {
            if (sender == tbFileLoad) {
                string fileName = AppHub.StdDialogs.GetOpenFile("", "", fPlugin.LangMan.LS(IVLS.LSID_FilesFilter), 1, "");
                if (!string.IsNullOrEmpty(fileName))
                {
                    SetFileRef(fileName);
                }
            }
        }

        private void SetFileRef(string fileName)
        {
            Text = fileName;
            Control ctl = null;
            fImageCtl = null;

            GEDCOMMultimediaFormat fmt = GEDCOMFileReference.RecognizeFormat(fileName);

            try
            {
                switch (fmt)
                {
                    case GEDCOMMultimediaFormat.mfBMP:
                    case GEDCOMMultimediaFormat.mfGIF:
                    case GEDCOMMultimediaFormat.mfJPG:
                    case GEDCOMMultimediaFormat.mfPCX:
                    case GEDCOMMultimediaFormat.mfTIF:
                    case GEDCOMMultimediaFormat.mfTGA:
                    case GEDCOMMultimediaFormat.mfPNG:
                        {
                            fImageCtl = new ImageView();

                            try {
                                using (Stream fs = new FileStream(fileName, FileMode.Open))
                                {
                                    fImageCtl.OpenImage(new Bitmap(fs));
                                }
                            } catch (Exception ex) {
                                Logger.LogWrite("ImageViewerWin.SetFileRef.0(): " + ex.Message);
                            }

                            ctl = fImageCtl;
                        }
                        break;

                    case GEDCOMMultimediaFormat.mfWAV:
                    case GEDCOMMultimediaFormat.mfAVI:
                    case GEDCOMMultimediaFormat.mfMPG:
                        break;

                    case GEDCOMMultimediaFormat.mfTXT:
                        {
                            TextBox txtBox = new TextBox();
                            txtBox.Multiline = true;
                            txtBox.ReadOnly = true;
                            txtBox.ScrollBars = ScrollBars.Both;

                            try {
                                using (Stream fs = new FileStream(fileName, FileMode.Open))
                                {
                                    using (StreamReader strd = new StreamReader(fs, Encoding.GetEncoding(1251)))
                                    {
                                        txtBox.Text = strd.ReadToEnd();
                                    }
                                }
                            } catch (Exception ex) {
                                Logger.LogWrite("ImageViewerWin.SetFileRef.1(): " + ex.Message);
                            }

                            ctl = txtBox;
                        }
                        break;

                    case GEDCOMMultimediaFormat.mfRTF:
                        {
                            RichTextBox rtfBox = new RichTextBox();
                            rtfBox.ReadOnly = true;

                            try {
                                using (Stream fs = new FileStream(fileName, FileMode.Open))
                                {
                                    using (StreamReader strd = new StreamReader(fs))
                                    {
                                        rtfBox.Text = strd.ReadToEnd();
                                    }
                                }
                            } catch (Exception ex) {
                                Logger.LogWrite("ImageViewerWin.SetFileRef.2(): " + ex.Message);
                            }

                            ctl = rtfBox;
                        }
                        break;

                    case GEDCOMMultimediaFormat.mfHTM:
                        {
                            var browser = new WebBrowser();
                            try {
                                using (Stream fs = new FileStream(fileName, FileMode.Open))
                                {
                                    browser.DocumentStream = fs;
                                }
                            } catch (Exception ex) {
                                Logger.LogWrite("ImageViewerWin.SetFileRef.2(): " + ex.Message);
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
            }
            catch (Exception ex)
            {
                if (ctl != null) ctl.Dispose();

                Logger.LogWrite("ImageViewerWin.SetFileRef()" + ex.Message);
            }
        }

        #region ILocalization support

        public void SetLang()
        {
            Text = fPlugin.LangMan.LS(IVLS.LSID_ImgViewer);
            tbFileLoad.ToolTipText = fPlugin.LangMan.LS(IVLS.LSID_FileLoad);

            /*if (fImageCtl != null) {
                fImageCtl.btnSizeToFit.Text = LangMan.LS(LSID.LSID_SizeToFit);
                fImageCtl.btnZoomIn.Text = LangMan.LS(LSID.LSID_ZoomIn);
                fImageCtl.btnZoomOut.Text = LangMan.LS(LSID.LSID_ZoomOut);
            }*/
        }

        #endregion
    }
}
