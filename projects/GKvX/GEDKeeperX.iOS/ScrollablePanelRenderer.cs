using System.Linq;
using GEDKeeperX.iOS;
using GKUI.Components;
using UIKit;
using Xamarin.Forms;
using Xamarin.Forms.Platform.iOS;

[assembly: ExportRenderer(typeof(ScrollablePanel), typeof(ScrollablePanelRenderer))]

namespace GEDKeeperX.iOS
{
    public class ScrollablePanelRenderer : ScrollViewRenderer
    {
        // bool zoomEnabled = false;

        protected override void OnElementChanged(VisualElementChangedEventArgs e)
        {
            base.OnElementChanged(e);
            MaximumZoomScale = 3f;
            MinimumZoomScale = 1.0f;
        }

        public override void LayoutSubviews()
        {
            base.LayoutSubviews();

            if (Subviews.Length > 0) {
                ViewForZoomingInScrollView += GetViewForZooming;
            } else {
                ViewForZoomingInScrollView -= GetViewForZooming;
            }
        }

        public UIView GetViewForZooming(UIScrollView sv)
        {
            return this.Subviews.FirstOrDefault();
        }
    }
}
