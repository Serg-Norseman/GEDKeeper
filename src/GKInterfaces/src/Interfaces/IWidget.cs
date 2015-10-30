
namespace GKCore.Interfaces
{
	public interface IWidget
	{
		IHost Host { get; } // ссылка на главную форму

		void BaseChanged(IBaseWindow aBase);
		void BaseClosed(IBaseWindow aBase);

		void WidgetInit(IHost host);
		void WidgetEnable();
	}
}
