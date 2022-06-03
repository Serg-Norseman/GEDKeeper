﻿/*
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
using System.ComponentModel;
using System.IO;
using BSLib.Design.Graphics;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GDModel;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public partial class MediaViewerWin : CommonWindow, IMediaViewerWin
    {
        private readonly MediaViewerController fController;
        private Control fViewer;

        public GDMFileReferenceWithTitle FileReference
        {
            get { return fController.FileReference; }
            set { fController.FileReference = value; }
        }

        public GDMMultimediaRecord MultimediaRecord
        {
            get { return fController.MultimediaRecord; }
            set { fController.MultimediaRecord = value; }
        }

        public void SetViewText(string text)
        {
            try {
                TextArea txtBox = new TextArea();
                txtBox.ReadOnly = true;
                // FIXME: fix encoding! and test other!!!
                txtBox.Text = text;

                SetViewControl(txtBox);
            } catch (Exception ex) {
                Logger.WriteError("MediaViewerWin.SetViewText()", ex);
            }
        }

        public void SetViewRTF(string text)
        {
            try {
                RichTextArea rtfBox = new RichTextArea();
                rtfBox.ReadOnly = true;
                rtfBox.Text = text;

                SetViewControl(rtfBox);
            } catch (Exception ex) {
                Logger.WriteError("MediaViewerWin.SetViewRTF()", ex);
            }
        }

        public void SetViewHTML(Stream stm)
        {
            try {
                var browser = new WebView();
                browser.LoadHtml(stm);

                SetViewControl(browser);
            } catch (Exception ex) {
                Logger.WriteError("MediaViewerWin.SetViewHTML()", ex);
            }
        }

        public void SetViewMedia(string mediaFile)
        {
            var mediaPlayer = new MediaPlayer();
            mediaPlayer.MediaFile = mediaFile;

            SetViewControl(mediaPlayer);
        }

        public void SetViewImage(IImage img, GDMFileReferenceWithTitle fileRef)
        {
            var imageCtl = new GKUI.Components.ImageView();
            imageCtl.OpenImage(img);

            fController.ProcessPortraits(imageCtl, fileRef);

            SetViewControl(imageCtl);
        }

        public void DisposeViewControl()
        {
            if (fViewer != null) fViewer.Dispose();
        }

        private void SetViewControl(Control ctl)
        {
            if (ctl == null) return;
            fViewer = ctl;
            SetLocale();

            SuspendLayout();
            Content = fViewer;
            ResumeLayout();
        }

        public MediaViewerWin(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            SetLocale();

            fController = new MediaViewerController(this);
            fController.Init(baseWin);
        }

        public override void SetLocale()
        {
            var localizable = fViewer as ILocalizable;
            if (localizable != null) localizable.SetLocale();
        }

        protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);
            if (fViewer != null) fViewer.Focus();
        }

        private void MediaViewerWin_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.Key == Keys.Escape) Close();
        }

        private void MediaViewerWin_FormClosing(object sender, CancelEventArgs e)
        {
            var mediaPlayer = fViewer as MediaPlayer;
            if (mediaPlayer != null) {
                mediaPlayer.btnStop_Click(null, null);
            }
        }
    }
}
