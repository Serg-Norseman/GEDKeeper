/*
 *  ULife
 *  Author: Ian Lane (email: lanei@ideal.net.au)
 *  Copyright (C) 1998 Ian Lane
 *
 *  Synopsis: A Delphi control which implements the old computer simulation
 *  of Life. Useful for about boxes, screen savers or even as the
 *  core of a "Life" application.
 *
 *  Distribution: This control is free for public use and components may be
 *  freely descended from it as long as credit is given to the author.
 * 
 *  Converted to C#: 20/07/2011, Serg V. Zhdanovskih
 */

using System.Drawing;
using System.Drawing.Drawing2D;

namespace GKLifePlugin
{
	public delegate void DoesCellLiveEvent(object sender, int x, int y, LifeGrid grid, ref bool result);
	
	public delegate void NotifyEvent(object sender);

	public static class LifeConsts
	{
		public const int MinGridHeight = 5;
		public const int MaxGridHeight = 250; // 46000
		public const int MinGridWidth = 5;
		public const int MaxGridWidth = 250; // 46000
		public const int AbsoluteMaxNumberOfHistoryLevels = int.MaxValue;

		public static Color DefaultCellColor = Color.LimeGreen;
		public static Color DefaultGridLineColor = SystemColors.WindowText;

		public const DashStyle DefaultGridLineStyle = DashStyle.Dot;
		public const int DefaultGridHeight = 100;
		public const int DefaultGridWidth = 100;
		public const int DefaultMaxNumberOfHistoryLevels = 10;

		public const int DefaultAnimationDelay = 750;

		public const bool DefaultDisplayGeneration = true;
		public const bool DefaultDisplayLivingCells = true;

		public static Color DefaultBackgroundColor = Color.Black;
		public static Color DefaultLivingCellColor = Color.Lime;
		public static Color DefaultDisplayedInformationColor = Color.Yellow;

		public static bool[] DefaultDeadCells = new bool[] {false, false, false, true, false, false, false, false, false};
		public static bool[] DefaultLiveCells = new bool[] {false, false, true, true, false, false, false, false, false};
	}
}
