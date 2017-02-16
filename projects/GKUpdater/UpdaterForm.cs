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
using System.Diagnostics;
using System.Drawing;
using System.Threading;
using System.Windows.Forms;

using GKCommon;

namespace GKUpdater
{
    public sealed class UpdaterForm : Form
    {
        private ProgressBar pgStatus;
        private Label lblStatus;

        private readonly Controller fController;

        public UpdaterForm()
        {
            Text = "GEDKeeper updater...";
            AutoScaleBaseSize = new Size(5, 13);
            BackColor = Color.White;
            ForeColor = Color.DarkBlue;
            ClientSize = new Size(300, 125);
            FormBorderStyle = FormBorderStyle.FixedToolWindow;
            StartPosition = FormStartPosition.CenterScreen;
            TopMost = true;
            Load += OnFormLoad;

            pgStatus = new ProgressBar();
            pgStatus.Parent = this;
            pgStatus.Location = new Point(25, 55);
            pgStatus.Size = new Size(250, 20);

            lblStatus = new Label();
            lblStatus.Parent = this;
            lblStatus.Text = "Loading application...";
            lblStatus.Location = new Point(25, 30);
            lblStatus.Size = new Size(250, 20);

            fController = Controller.Instance;
            fController.UpdateMessage += OnUpdateMessage;
            fController.DownloadProgress += OnUpdateMessage;
        }

        private void WorkerMethod()
        {
            if (!string.IsNullOrEmpty(Controller.Url))
            {
                bool success = false;

                try {
                    fController.CheckForUpdates(Controller.Url);

                    if (fController.NeedsUpdate)
                    {
                        if (MessageBox.Show("Updates are available. You should update this version! Do you want to run the automatic update?", "Updates available", MessageBoxButtons.YesNo, MessageBoxIcon.Information) == DialogResult.Yes)
                        {
                            if (fController.Update()) {
                                MessageBox.Show("The update was successful. This version is now up to date.", "Update successful!", MessageBoxButtons.OK, MessageBoxIcon.Information);
                                success = true;
                            } else
                                MessageBox.Show("An unexpected error has occurred. The update was canceled and no files were changed. It could help to visit the product home page directly.", "Update failed!", MessageBoxButtons.OK, MessageBoxIcon.Information);
                        }
                    }
                } catch (Exception ex) {
                    Logger.LogWrite("UpdaterForm.WorkerMethod(): " + ex.Message);
                }

                if (fController.NeedsUpdate && !success)
                {
                    if (MessageBox.Show("An unexpected error has occurred. Do you want to visit update website?", "Manual update required", MessageBoxButtons.YesNo, MessageBoxIcon.Warning) == DialogResult.Yes)
                        Process.Start(Controller.ProductUrl);
                }
            }

            ThreadedClose();
        }

        private delegate void ProgressHandler(UpdateEventArgs uea);
        private delegate void CloseHandler();

        private void ThreadSafeProgress(UpdateEventArgs uea)
        {
            lblStatus.Text = uea.Message;
            if (uea.Progress >= 0 && uea.Progress <= 100)
                pgStatus.Value = uea.Progress;
        }

        private void OnUpdateMessage(object sender, UpdateEventArgs uea)
        {
            if (InvokeRequired)
                Invoke(new ProgressHandler(ThreadSafeProgress), new object[] {uea});
            else
                ThreadSafeProgress(uea);
        }

        private void ThreadSafeClose()
        {
            Close();
        }

        private void ThreadedClose()
        {
            if (InvokeRequired)
                Invoke(new CloseHandler(ThreadSafeClose), new object[] {});
            else
                ThreadSafeClose();
        }

        private void OnFormLoad(object sender, EventArgs ea)
        {
            Thread worker = new Thread(WorkerMethod);
            worker.SetApartmentState(ApartmentState.STA);
            worker.IsBackground = true;
            worker.Start();
        }

        protected override void OnClosed(EventArgs e)
        {
            base.OnClosed(e);
            Controller.StartMainApp();
        }
    }
}
