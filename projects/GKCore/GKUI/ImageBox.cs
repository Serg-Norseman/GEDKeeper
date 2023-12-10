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

    public static class ImageBoxConstants
    {
        public const int MAX_ZOOM = 3500;
        public const int MIN_ZOOM = 1;
        public const int SELECTION_DEAD_ZONE = 5;
    }
}
