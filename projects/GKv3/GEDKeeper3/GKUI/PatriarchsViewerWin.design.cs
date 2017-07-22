using Eto.Drawing;
using GKUI.Components;

namespace GKUI
{
    partial class PatriarchsViewerWin
    {
        private ArborViewer arborViewer1;

        private void InitializeComponent()
        {
            arborViewer1 = new ArborViewer();
            arborViewer1.BackgroundColor = Colors.White;
            //arborViewer1.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
            arborViewer1.EnergyDebug = false;
            arborViewer1.NodesDragging = false;
            arborViewer1.MouseMove += ArborViewer1_MouseMove;

            ClientSize = new Size(800, 600);
            Content = arborViewer1;
            Title = "PatriarchsViewer";
        }
    }
}
