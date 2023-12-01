using Android.Content;
using Android.Views;
using GEDKeeperX.Droid.Renderers;
using GKUI.Components;
using Xamarin.Forms;
using Xamarin.Forms.Platform.Android;
using static Android.Views.ScaleGestureDetector;

[assembly: ExportRenderer(typeof(ScrollablePanel), typeof(ScrollablePanelRenderer))]

namespace GEDKeeperX.Droid.Renderers
{
    public class ScrollablePanelRenderer : ScrollViewRenderer, IOnScaleGestureListener
    {
        private float fScale = 1f;
        private ScaleGestureDetector fScaleDetector;

        public ScrollablePanelRenderer(Context context) : base(context)
        {
        }

        protected override void OnElementChanged(VisualElementChangedEventArgs e)
        {
            base.OnElementChanged(e);
            fScaleDetector = new ScaleGestureDetector(Context, this);
        }

        public override bool DispatchTouchEvent(MotionEvent e)
        {
            base.DispatchTouchEvent(e);
            return fScaleDetector.OnTouchEvent(e);
        }

        public bool OnScale(ScaleGestureDetector detector)
        {
            float scale = 1 - detector.ScaleFactor;
            fScale -= scale;

            ((ScrollablePanel)Element).OnZoom(new ZoomEventArgs(fScale, Point.Zero));

            return true;
        }

        public bool OnScaleBegin(ScaleGestureDetector detector)
        {
            return true;
        }

        public void OnScaleEnd(ScaleGestureDetector detector)
        {
        }
    }
}
