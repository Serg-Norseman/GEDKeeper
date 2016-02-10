using System;
using System.Windows.Forms;
using System.Security.Permissions;

namespace GKCommon.Controls
{
	/// <summary>
	/// Description of WizardPages.
	/// </summary>
	public class WizardPages : TabControl
	{
        [SecurityPermission(SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.UnmanagedCode), SecurityPermission(SecurityAction.InheritanceDemand, Flags = SecurityPermissionFlag.UnmanagedCode)]
		protected override void WndProc(ref Message m)
		{
			// Hide tabs by trapping the TCM_ADJUSTRECT message
			if (m.Msg == 0x1328 && !DesignMode) {
				m.Result = (IntPtr)1;
			} else {
				base.WndProc(ref m);
			}
		}

	}
}
