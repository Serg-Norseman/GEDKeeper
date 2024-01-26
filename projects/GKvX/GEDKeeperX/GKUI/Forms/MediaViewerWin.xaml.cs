/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using GDModel;
using GKCore;
using GKCore.Controllers;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKCore.Interfaces;
using Xamarin.Forms;

namespace GKUI.Forms
{
    public partial class MediaViewerWin : CommonWindow, IMediaViewerWin
    {
        private readonly MediaViewerController fController;
        private View fViewer;

        public GDMMultimediaRecord MultimediaRecord
        {
            get { return fController.MultimediaRecord; }
            set { fController.MultimediaRecord = value; }
        }

        public IWindow OwnerWindow
        {
            get { return fController.Base; }
        }


        public MediaViewerWin(IBaseWindow baseWin)
        {
            InitializeComponent();

            SetLocale();

            fController = new MediaViewerController(this);
            fController.Init(baseWin);
        }

        public override void SetLocale()
        {
            var localizable = fViewer as ILocalizable;
            if (localizable != null) localizable.SetLocale();
        }

        protected override void OnAppearing()
        {
            base.OnAppearing();
            if (fViewer != null) fViewer.Focus();
        }

        private void MediaViewerWin_FormClosing(object sender, CancelEventArgs e)
        {
#if !DIS_VLC
            /*var mediaPlayer = fViewer as MediaPlayer;
            if (mediaPlayer != null) {
                mediaPlayer.btnStop_Click(null, null);
            }*/
#endif
        }

        public void SetViewText(string text)
        {
            try {
                var txtBox = new Editor();
                txtBox.IsReadOnly = true;
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
                // https://www.nuget.org/packages/RtfPipe
                /*RichTextArea rtfBox = new RichTextArea();
                rtfBox.ReadOnly = true;
                rtfBox.Text = text;

                SetViewControl(rtfBox);*/
            } catch (Exception ex) {
                Logger.WriteError("MediaViewerWin.SetViewRTF()", ex);
            }
        }

        public void SetViewHTML(Stream stm)
        {
            try {
                StreamReader reader = new StreamReader(stm);
                string htmlString = reader.ReadToEnd();

                var browser = new WebView();
                var htmlSource = new HtmlWebViewSource();
                htmlSource.Html = htmlString;
                browser.Source = htmlSource;

                SetViewControl(browser);
            } catch (Exception ex) {
                Logger.WriteError("MediaViewerWin.SetViewHTML()", ex);
            }
        }

        public void SetViewMedia(string mediaFile)
        {
#if !DIS_VLC
            /*var mediaPlayer = new MediaPlayer();
            mediaPlayer.MediaFile = mediaFile;

            SetViewControl(mediaPlayer);*/
#endif
        }

        public void SetViewImage(IImage img)
        {
            var imageCtl = new GKUI.Components.ImageView();
            imageCtl.OpenImage(fController, img);

            fController.ProcessPortraits(imageCtl);

            SetViewControl(imageCtl);
        }

        public void DisposeViewControl()
        {
        }

        private void SetViewControl(View ctl)
        {
            if (ctl == null) return;
            fViewer = ctl;
            SetLocale();

            Content = fViewer;
        }
    }
}
