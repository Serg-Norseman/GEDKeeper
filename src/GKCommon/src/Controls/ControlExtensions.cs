using System;
using System.Windows.Forms;

namespace GKCommon.Controls
{
	public static class ControlExtensions
	{
		public static void Do<T>(T control, Action<T> action) where T: Control
		{
			if (control.InvokeRequired)
				control.Invoke(action, control);
			else
				action(control);
		}
	}
}
