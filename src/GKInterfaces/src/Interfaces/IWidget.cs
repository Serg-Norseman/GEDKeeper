using System.Windows.Forms;

namespace GKCore.Interfaces
{
	public interface IWidget
	{
		IHost Host { get; } // ссылка на главную форму
		MenuItem MenuItem { get; }

		void BaseChanged(IBase aBase);
		void WidgetInit(IHost host, MenuItem menuItem);
		void WidgetEnable();
	}
}
