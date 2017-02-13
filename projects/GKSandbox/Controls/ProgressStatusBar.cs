using System.Drawing;
using System.Windows.Forms;

namespace GKCommon.Controls
{
    public class ProgressStatusBar : StatusBar
    {
        private delegate void StatusTextSetter(string msg);
        private delegate void StatusProgressSetter(int val);

        private readonly ProgressBar fProgressBar;
        
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
            get { return fProgressBar.Value; }
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
            DrawItem += Reposition;

            fProgressBar = new ProgressBar();
            fProgressBar.Value = 0;
            fProgressBar.Hide();
            Controls.Add(fProgressBar);
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
                fProgressBar.Hide();
            }
            else
            {
                Panels[1].Style = StatusBarPanelStyle.OwnerDraw;
                fProgressBar.Show();
            }
            fProgressBar.Value = val;
        }

        private void Reposition(object sender, StatusBarDrawItemEventArgs eArgs)
        {
            fProgressBar.Location = new Point(eArgs.Bounds.X, eArgs.Bounds.Y);
            fProgressBar.Size = new Size(eArgs.Bounds.Width, eArgs.Bounds.Height);
            fProgressBar.Show();
        }
    }
}
