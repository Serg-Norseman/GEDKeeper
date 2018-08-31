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
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;

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
    public partial class MediaViewerWin : CommonForm, ILocalization
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
                            IImage img = fBase.Context.LoadMediaImage(fFileRef, false);
                            if (img != null) {
                                SetViewImage(((ImageHandler)img).Handle, fFileRef);
                            }
                            break;
                        }

                    case MultimediaKind.mkAudio:
                    case MultimediaKind.mkVideo:
                        {
                            string targetFile = fBase.Context.MediaLoad(fFileRef);
                            SetViewMedia(targetFile);
                            break;
                        }

                    case MultimediaKind.mkText:
                        {
                            Stream fs = fBase.Context.MediaLoad(fFileRef, false);

                            switch (fFileRef.MultimediaFormat)
                            {
                                case GEDCOMMultimediaFormat.mfTXT:
                                    {
                                        TextBox txtBox = new TextBox();
                                        txtBox.Multiline = true;
                                        txtBox.ReadOnly = true;
                                        txtBox.ScrollBars = ScrollBars.Both;

                                        try {
                                            // FIXME: fix encoding! and test other!!!
                                            using (StreamReader strd = new StreamReader(fs, Encoding.GetEncoding(1251))) {
                                                txtBox.Text = strd.ReadToEnd();
                                            }
                                        } catch (Exception ex) {
                                            Logger.LogWrite("MediaViewerWin.SetFileRef.1(): " + ex.Message);
                                        }

                                        ctl = txtBox;
                                        SetViewControl(ctl);
                                    }
                                    break;

                                case GEDCOMMultimediaFormat.mfRTF:
                                    {
                                        RichTextBox rtfBox = new RichTextBox();
                                        rtfBox.ReadOnly = true;

                                        try {
                                            using (StreamReader strd = new StreamReader(fs)) {
                                                rtfBox.Text = strd.ReadToEnd();
                                            }
                                        } catch (Exception ex) {
                                            Logger.LogWrite("MediaViewerWin.SetFileRef.2(): " + ex.Message);
                                        }

                                        ctl = rtfBox;
                                        SetViewControl(ctl);
                                    }
                                    break;

                                case GEDCOMMultimediaFormat.mfHTM:
                                    {
                                        var browser = new WebBrowser();

                                        try {
                                            browser.DocumentStream = fs;
                                            /*using (StreamReader strd = new StreamReader(fs)) {
                                                browser.DocumentText = strd.ReadToEnd();
                                                // didn't work, because didn't defines codepage from page's header (?!)
                                            }*/
                                        } catch (Exception ex) {
                                            Logger.LogWrite("MediaViewerWin.SetFileRef.3(): " + ex.Message);
                                        }

                                        ctl = browser;
                                        SetViewControl(ctl);
                                    }
                                    break;
                            }
                            if (fs != null && !(ctl is WebBrowser)) fs.Dispose();
                            
                            break;
                        }
                }
            }
            catch (Exception ex)
            {
                if (ctl != null) ctl.Dispose();

                Logger.LogWrite("MediaViewerWin.SetFileRef(): " + ex.Message);
            }
        }

        public void SetViewMedia(string mediaFile)
        {
            MediaPlayer mediaPlayer = new MediaPlayer();
            mediaPlayer.MediaFile = mediaFile;

            SetViewControl(mediaPlayer);
        }

        public void SetViewImage(Image img, GEDCOMFileReferenceWithTitle fileRef)
        {
            ImageView imageCtl = new ImageView();
            imageCtl.OpenImage(img);

            ProcessPortraits(imageCtl, fileRef);

            SetViewControl(imageCtl);
        }

        private void ProcessPortraits(ImageView imageCtl, GEDCOMFileReferenceWithTitle fileRef)
        {
            var mmRec = fileRef.Parent as GEDCOMMultimediaRecord;

            var linksList = new List<GEDCOMObject>();
            GKUtils.SearchRecordLinks(linksList, mmRec.Owner, mmRec);

            foreach (var link in linksList) {
                var mmLink = link as GEDCOMMultimediaLink;
                if (mmLink != null && mmLink.IsPrimary) {
                    var indiRec = mmLink.Parent as GEDCOMIndividualRecord;
                    string indiName = GKUtils.GetNameString(indiRec, true, false);
                    var region = UIHelper.Rt2Rt(mmLink.CutoutPosition.Value);

                    imageCtl.NamedRegions.Add(new NamedRegion(indiName, region));
                }
            }

            imageCtl.ShowNamedRegionTips = (imageCtl.NamedRegions.Count > 0);
        }

        private void SetViewControl(Control ctl)
        {
            if (ctl == null) return;
            fViewer = ctl;
            SetLang();

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
            var localizable = fViewer as ILocalization;
            if (localizable != null) localizable.SetLang();
        }

        protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);
            if (fViewer != null) fViewer.Select();
        }

        private void MediaViewerWin_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Escape) Close();
        }

        private void MediaViewerWin_FormClosing(object sender, FormClosingEventArgs e)
        {
            var mediaPlayer = fViewer as MediaPlayer;
            if (mediaPlayer != null) {
                mediaPlayer.btnStop_Click(null, null);
            }
        }
    }
}
