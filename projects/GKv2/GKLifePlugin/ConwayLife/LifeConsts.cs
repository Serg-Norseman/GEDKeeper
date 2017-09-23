/*
 *  ULife, the old computer simulation of Life.
 *  Copyright (C) 1998 by Ian Lane (email: lanei@ideal.net.au)
 *
 *  Distribution: This control is free for public use and components may be
 *  freely descended from it as long as credit is given to the author.
 * 
 *  Converted to C#: 20/07/2011, Serg V. Zhdanovskih
 */

using System.Drawing;
using System.Drawing.Drawing2D;

namespace ConwayLife
{
    public delegate void DoesCellLiveEvent(object sender, int x, int y, LifeGrid grid, ref bool result);
    
    public delegate void NotifyEvent(object sender);

    public static class LifeConsts
    {
        public const int MinGridHeight = 5;
        public const int MaxGridHeight = 1000;
        public const int MinGridWidth = 5;
        public const int MaxGridWidth = 1000;

        public const int DefaultGridHeight = 200;
        public const int DefaultGridWidth = 200;

        public const int MaxNumberOfHistoryLevels = byte.MaxValue;
        public const int DefaultNumberOfHistoryLevels = 10;

        public const int DefaultAnimationDelay = 100;

        public static Color DefaultCellColor = Color.Green;
        public static Color DefaultBackgroundColor = Color.Silver;

        public static Color DefaultGridLineColor = Color.Black;
        public const DashStyle DefaultGridLineStyle = DashStyle.Dot;

        public static bool[] DefaultDeadCells = new bool[] {false, false, false, true, false, false, false, false, false};
        public static bool[] DefaultLiveCells = new bool[] {false, false, true, true, false, false, false, false, false};
    }
}
