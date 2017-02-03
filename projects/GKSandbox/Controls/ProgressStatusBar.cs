using System;
using System.Drawing;
using System.Windows.Forms;

namespace GKCommon.Controls
{
    public class ProgressStatusBar : StatusBar
    {
        private delegate void StatusTextSetter(string msg);
        private delegate void StatusProgressSetter(int val);

        private ProgressBar progressBar;
        
        public string StatusText
        {
            get { return Panels[0].Text; }
            set
            {
                if (InvokeRequired)
                    Invoke(new StatusTextSetter(ThreadSafeInfoSetter), new object[] { value });
                else
                    ThreadSafeInfoSetter(value);
            }
        }
        
        public int Progress
        {
            get { return progressBar.Value; }
            set
            {
                if (InvokeRequired)
                    Invoke(new StatusProgressSetter(ThreadSafeProgressSetter), new object[] { value });
                else
                    ThreadSafeProgressSetter(value);
            }
        }

        public ProgressStatusBar()
        {
            ShowPanels = true;
            Panels.Add(new StatusBarPanel());
            Panels.Add(new StatusBarPanel());
            Panels[0].Width = 150;
            Panels[0].Text = "Progress";
            Panels[1].AutoSize = StatusBarPanelAutoSize.Spring;
            DrawItem += new StatusBarDrawItemEventHandler(Reposition);

            progressBar = new ProgressBar();
            progressBar.Value = 0;
            progressBar.Hide();
            Controls.Add(progressBar);
        }

        private void ThreadSafeInfoSetter(string msg)
        {
            Panels[0].Text = msg;
        }

        protected void ThreadSafeProgressSetter(int val)
        {
            if (val == 0)
            {
                Panels[1].Style = StatusBarPanelStyle.Text;
                progressBar.Hide();
            }
            else
            {
                Panels[1].Style = StatusBarPanelStyle.OwnerDraw;
                progressBar.Show();
            }
            progressBar.Value = val;
        }

        private void Reposition(object sender, StatusBarDrawItemEventArgs eArgs)
        {
            progressBar.Location = new System.Drawing.Point(eArgs.Bounds.X, eArgs.Bounds.Y);
            progressBar.Size = new System.Drawing.Size(eArgs.Bounds.Width, eArgs.Bounds.Height);
            progressBar.Show();
        }
    }
}
