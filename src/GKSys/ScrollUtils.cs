using System;
using System.Windows.Forms;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKSys
{
	public sealed class ScrollUtils
	{
		private static readonly ScrollEventType[] _events;

		public ScrollUtils()
		{
		}

		static ScrollUtils()
		{
			_events = new ScrollEventType[]
			{
				ScrollEventType.SmallDecrement, 
				ScrollEventType.SmallIncrement, 
				ScrollEventType.LargeDecrement, 
				ScrollEventType.LargeIncrement, 
				ScrollEventType.ThumbPosition, 
				ScrollEventType.ThumbTrack, 
				ScrollEventType.First, 
				ScrollEventType.Last, 
				ScrollEventType.EndScroll
			};
		}

		public static ScrollEventType GetScrollEventType(uint wParam)
		{
			ScrollEventType Result = ((wParam <= 8u) ? _events[(int)wParam] : ScrollEventType.EndScroll);
			return Result;
		}

	}
}
