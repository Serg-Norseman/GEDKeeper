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
using GKCore.Interfaces;

namespace GKImageViewerPlugin
{
    /// <summary>
    /// 
    /// </summary>
    public partial class ImageViewerWin : Form, ILocalization
    {
        private ImageView fImageCtl;
        private readonly ILangMan fLangMan;

        public ImageViewerWin(IPlugin plugin)
        {
            this.InitializeComponent();

            this.tbFileLoad.Image = IVPResource.iLoad;
            
            this.fLangMan = plugin.LangMan;

            this.SetLang();
        }

        private void TfmMediaView_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Escape)
            {
                base.Close();
            }
        }

        private void ToolBar1_ButtonClick(object sender, EventArgs e)
        {
            if (sender == this.tbFileLoad) {
                if (OpenDialog1.ShowDialog() == DialogResult.OK) {
                    this.SetFileRef(OpenDialog1.FileName);
                }
            }
        }

        private void SetFileRef(string fileName)
        {
            this.SuspendLayout();

            this.Text = fileName;
            Control ctl = null;
            this.fImageCtl = null;

            GEDCOMMultimediaFormat fmt = GEDCOMFileReference.RecognizeFormat(fileName);
            
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
                        using (Stream fs = new FileStream(fileName, FileMode.Open))
                        {
                            this.fImageCtl = new ImageView();
                            this.fImageCtl.OpenImage(new Bitmap(fs));
                            ctl = this.fImageCtl;
                        }
                        break;
                    }

                case GEDCOMMultimediaFormat.mfWAV:
                case GEDCOMMultimediaFormat.mfAVI:
                case GEDCOMMultimediaFormat.mfMPG:
                    break;

                case GEDCOMMultimediaFormat.mfTXT:
                    {
                        using (Stream fs = new FileStream(fileName, FileMode.Open))
                        {
                            using (StreamReader strd = new StreamReader(fs, Encoding.GetEncoding(1251)))
                            {
                                TextBox txtBox = new TextBox();
                                txtBox.Multiline = true;
                                txtBox.ReadOnly = true;
                                txtBox.ScrollBars = ScrollBars.Both;
                                txtBox.Text = strd.ReadToEnd();
                                ctl = txtBox;
                            }
                        }
                        break;
                    }

                case GEDCOMMultimediaFormat.mfRTF:
                    {
                        using (Stream fs = new FileStream(fileName, FileMode.Open))
                        {
                            using (StreamReader strd = new StreamReader(fs))
                            {
                                RichTextBox txtBox = new RichTextBox();
                                txtBox.ReadOnly = true;
                                txtBox.Text = strd.ReadToEnd();
                                ctl = txtBox;
                            }
                        }
                        break;
                    }

                case GEDCOMMultimediaFormat.mfHTM:
                    {
                        using (Stream fs = new FileStream(fileName, FileMode.Open))
                        {
                            ctl = new WebBrowser();
                            (ctl as WebBrowser).DocumentStream = fs;
                        }
                        break;
                    }
            }

            if (ctl != null) {
                ctl.Dock = DockStyle.Fill;
                ctl.Location = new Point(0, 50);
                ctl.Size = new Size(100, 100);
                base.Controls.Add(ctl);
                base.Controls.SetChildIndex(ctl, 0);
            }

            this.ResumeLayout(false);
            this.PerformLayout();
        }

        #region ILocalization support

        public void SetLang()
        {
            this.Text = this.fLangMan.LS(IVLS.LSID_ImgViewer);
            
            /*if (this.fImageCtl != null) {
                this.fImageCtl.btnSizeToFit.Text = LangMan.LS(LSID.LSID_SizeToFit);
                this.fImageCtl.btnZoomIn.Text = LangMan.LS(LSID.LSID_ZoomIn);
                this.fImageCtl.btnZoomOut.Text = LangMan.LS(LSID.LSID_ZoomOut);
            }*/
        }

        #endregion
    }
}
