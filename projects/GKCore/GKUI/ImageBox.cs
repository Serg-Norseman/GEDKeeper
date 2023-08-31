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

    public static class ImageBoxConstants
    {
        public const int MAX_ZOOM = 3500;
        public const int MIN_ZOOM = 1;
        public const int SELECTION_DEAD_ZONE = 5;
    }
}
