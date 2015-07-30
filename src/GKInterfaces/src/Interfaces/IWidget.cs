
namespace GKCore.Interfaces
{
	public interface IWidget
	{
		IHost Host { get; } // ссылка на главную форму

		void BaseChanged(IBase aBase);
		void BaseClosed(IBase aBase);

		void WidgetInit(IHost host);
		void WidgetEnable();
	}
}
