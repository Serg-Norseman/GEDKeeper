/*
 * Created by SharpDevelop.
 * User: Norseman
 * Date: 08.06.2017
 * Time: 17:42
 * 
 * To change this template use Tools | Options | Coding | Edit Standard Headers.
 */
using System;
using Eto.Drawing;
using Eto.Forms;
using GKCommon;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class ScrollablePanelStub : CustomPanel
    {
        public const int SmallChange = 1;
        public const int LargeChange = 10;

        public Font Font {
            get;
            set;
        }

        public Point AutoScrollPosition
        {
            get { return Point.Empty; }
            set {  }
        }

        public Color TextColor {
            get;
            set;
        }

        public int HorizontalScrollValue
        {
            get;
            set;
        }

        public int VerticalScrollValue
        {
            get;
            set;
        }

        public ScrollablePanelStub()
        {
        }
        
        public void AdjustViewPort(ExtSize sz, bool noRedraw = true)
        {
            
        }
        
        public void AdjustScroll(int x, int y)
        {
            
        }
    }
}
