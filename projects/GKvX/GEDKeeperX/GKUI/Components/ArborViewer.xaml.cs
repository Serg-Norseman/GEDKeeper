using BSLib.DataViz.ArborGVT;
using Xamarin.Forms;

namespace GKUI.Components
{
    /// <summary>
    /// Stub component
    /// </summary>
    public sealed partial class ArborViewer : View
    {
        public Color BackColor
        {
            get { return Color.White; }
            set { }
        }

        public bool EnergyDebug
        {
            get { return false; }
            set { }
        }

        public bool NodesDragging
        {
            get { return false; }
            set { }
        }

        public ArborSystem Sys
        {
            get { return null; }
            set { }
        }

        public ArborViewer()
        {
            //this.InitializeComponent();
        }

        public void start()
        {

        }
    }
}
