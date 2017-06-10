/*
 * Created by SharpDevelop.
 * User: Norseman
 * Date: 08.06.2017
 * Time: 17:48
 * 
 * To change this template use Tools | Options | Coding | Edit Standard Headers.
 */
using System;
using Eto.Drawing;
using Eto.Forms;

namespace GKUI.Components
{
    /// <summary>
    ///   Specifies the selection mode.
    /// </summary>
    public enum ImageBoxSelectionMode
    {
        /// <summary>
        ///   No selection.
        /// </summary>
        None,

        /// <summary>
        ///   Rectangle selection.
        /// </summary>
        Rectangle,

        /// <summary>
        ///   Zoom selection.
        /// </summary>
        Zoom
    }

    /// <summary>
    ///   Specifies the border styles of an image
    /// </summary>
    public enum ImageBoxBorderStyle
    {
        /// <summary>
        ///   No border.
        /// </summary>
        None,

        /// <summary>
        ///   A fixed, single-line border.
        /// </summary>
        FixedSingle,

        /// <summary>
        ///   A fixed, single-line border with a solid drop shadow.
        /// </summary>
        FixedSingleDropShadow,

        /// <summary>
        ///   A fixed, single-line border with a soft outer glow.
        /// </summary>
        FixedSingleGlowShadow
    }

    /// <summary>
    /// Description of ImageBoxStub.
    /// </summary>
    public class ImageBoxStub : Scrollable
    {
        public ImageBoxSelectionMode SelectionMode
        {
            get;
            set;
        }
        
        public RectangleF SelectionRegion
        {
            get;
            set;
        }
        
        public Image Image
        {
            get;
            set;
        }

        public ImageBoxStub()
        {
        }

        public void BeginUpdate()
        {
        }

        public void EndUpdate()
        {
        }

        public void ZoomIn()
        {
        }

        public void ZoomOut()
        {
        }

        public void ZoomToFit()
        {
        }

        public void x()
        {
        }
    }
}
