using System;
using System.Windows.Forms;

namespace ExtUtils.Controls
{
	public static class ControlExtensions
	{
		public static void Do<TControl>(this TControl control, Action<TControl> action) where TControl: Control
		{
			if (control.InvokeRequired)
				control.Invoke(action, control);
			else
				action(control);
		}
	}
}
