using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Imaging;
using System.Windows.Forms;

using CsGL.OpenGL;
using ExtUtils;
using ExtUtils.ArborEngine;
using GKCommon;
using GKCommon.GEDCOM;
using GKCommon.GEDCOM.Enums;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKTreeVizPlugin
{
	public sealed class TreeVizControl : OpenGLControl
	{
		private const bool EXCLUDE_CHILDLESS = true;	// исключение бездетных ветвей древа
		private const float MAGIC_SCALE = 4;			// коэффициент масштабирования при переходе от Arbor к 3D
		private const float DEG2RAD = 3.14159F / 180;
		private const uint OBJ_X = 1;
		private const uint OBJ_Y = 2;
		private const uint OBJ_Z = 3;
		private const uint OBJ_NODE = 100;

		private const float BaseScale = 10.0f;
		

		private enum TVPersonType {
			ptStem, 
			ptSpouse,
			ptPatriarch,
		}

		private class TVPerson
		{
			private static int NextIdx = 1;
			private static readonly Random random = new Random();
			
			public readonly int Idx;
			public TVPerson Parent;
			public GEDCOMIndividualRecord IRec;
			public PointF Pt;
			public int BirthYear, DeathYear;
			public float BaseRadius;

			public TVPersonType Type;

			//public bool IsPatriarch;
			public int DescGenerations;
			public float GenSlice;
			public int BeautySpouses, BeautyChilds;
			
			public List<TVPerson> Spouses { get; private set; }
			public List<TVPerson> Childs { get; private set; }
			
			public TVPerson()
			{
				this.Idx = NextIdx++;
				this.Spouses = new List<TVPerson>();
				this.Childs = new List<TVPerson>();
				this.BeautySpouses = random.Next(0, 360);
				this.BeautyChilds = random.Next(0, 360);
			}
		}
		
		// unused
		//private float[] LightAmbient = {0.5f, 0.5f, 0.5f, 1.0f};
		//private float[] LightDiffuse = {1.0f, 1.0f, 1.0f, 1.0f};
		//private float[] LightPosition = {0.0f, 0.0f, 2.0f, 1.0f};
		//private int filter = 0;													// Which Filter To Use
		//private uint[] texture = new uint[3];									// Storage For 3 Textures

		// rendering
		private byte accumDepth = 0;												// OpenGL's Accumulation Buffer Depth, In Bits Per Pixel.
		private byte stencilDepth = 0;											// OpenGL's Stencil Buffer Depth, In Bits Per Pixel.
		private byte zDepth = 16;												// OpenGL's Z-Buffer Depth, In Bits Per Pixel.
		private byte colorDepth = 16;											// The Current Color Depth, In Bits Per Pixel.
		private double nearClippingPlane = 0.1f;									// GLU's Distance From The Viewer To The Near Clipping Plane (Always Positive).
		private double farClippingPlane = 100.0f;								// GLU's Distance From The Viewer To The Far Clipping Plane (Always Positive).
		private double fovY = 45.0f;												// GLU's Field Of View Angle, In Degrees, In The Y Direction.
		private float xrot = 0;
		private float yrot = 0;
		private float zrot = 0;
		private float z = -5.0f;

		// control
		private int fHeight;
		private int fWidth;
		private bool fMouseDrag;
		private int fLastX;
		private int fLastY;
		private bool fFreeRotate;

		// runtime
		private IBase fBase;
		private ArborSystem fSys;
		private readonly List<TVPerson> fPersons;
		private readonly Dictionary<string, TVPerson> fPersonsIndex;
		private int fMinYear;
		private float fYearSize;

		// animation
		private System.Timers.Timer fAnimTimer = null;
		private bool fBusy;
		private int fCurYear;
		private uint fTick;


		public bool Debug { get; set; }
		public bool TimeStop { get; set; }
		public string SelectedObject { get; private set; }
		
		public bool FreeRotate
		{
			get {
				return this.fFreeRotate;
			}
			set {
				this.fFreeRotate = value;

				if (!value) {
					this.xrot = -75;
					this.yrot = 0.0F;
				} else {
				}
			}
		}


		public TreeVizControl()
		{
			this.Dock = DockStyle.Fill;

			GL.glShadeModel(GL.GL_SMOOTH);
			GL.glClearColor(0.0f, 0.0f, 0.0f, 0.5f);
			GL.glClearDepth(1.0f);
			GL.glEnable(GL.GL_DEPTH_TEST);
			GL.glDepthFunc(GL.GL_LEQUAL);
			GL.glHint(GL.GL_PERSPECTIVE_CORRECTION_HINT, GL.GL_NICEST);

			//glEnable(GL_TEXTURE_2D);
			//LoadTextures();
			//GL.glLightfv(GL.GL_LIGHT1, GL.GL_AMBIENT, this.LightAmbient);
			//GL.glLightfv(GL.GL_LIGHT1, GL.GL_DIFFUSE, this.LightDiffuse);
			//GL.glLightfv(GL.GL_LIGHT1, GL.GL_POSITION, this.LightPosition);
			//GL.glEnable(GL.GL_LIGHT1);

			this.Context.Grab();
			OpenGLException.Assert();

			this.fPersons = new List<TVPerson>();
			this.fPersonsIndex = new Dictionary<string, TVPerson>();
			
			this.Debug = true;
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing) {
				if (fSys != null) fSys.Dispose();
			}
			base.Dispose(disposing);
		}

		#region Control routines

		public void Redraw()
		{
			OnPaint(null);
		}

		public override void glDraw()
		{
			try
			{
				GL.glClear(GL.GL_COLOR_BUFFER_BIT | GL.GL_DEPTH_BUFFER_BIT);
				GL.glLoadIdentity();

				GL.glInitNames();

				GL.glTranslatef(0.0f, 0.0f, this.z);
				GL.glRotatef(this.xrot, 1.0f, 0.0f, 0.0f);
				GL.glRotatef(this.yrot, 0.0f, 1.0f, 0.0f);
				GL.glRotatef(this.zrot, 0.0f, 0.0f, 1.0f);

				/*glBindTexture(GL_TEXTURE_2D, texture[filter]);
				glBegin(GL_QUADS);
				// Front Face
				glNormal3f(0.0f, 0.0f, 1.0f);
				glTexCoord2f(0.0f, 0.0f); glVertex3f(-1.0f, -1.0f,  1.0f);
				glTexCoord2f(1.0f, 0.0f); glVertex3f( 1.0f, -1.0f,  1.0f);
				glTexCoord2f(1.0f, 1.0f); glVertex3f( 1.0f,  1.0f,  1.0f);
				glTexCoord2f(0.0f, 1.0f); glVertex3f(-1.0f,  1.0f,  1.0f);
				// Back Face
				glNormal3f(0.0f, 0.0f, -1.0f);
				glTexCoord2f(1.0f, 0.0f); glVertex3f(-1.0f, -1.0f, -1.0f);
				glTexCoord2f(1.0f, 1.0f); glVertex3f(-1.0f,  1.0f, -1.0f);
				glTexCoord2f(0.0f, 1.0f); glVertex3f( 1.0f,  1.0f, -1.0f);
				glTexCoord2f(0.0f, 0.0f); glVertex3f( 1.0f, -1.0f, -1.0f);
				// Top Face
				glNormal3f(0.0f, 1.0f, 0.0f);
				glTexCoord2f(0.0f, 1.0f); glVertex3f(-1.0f,  1.0f, -1.0f);
				glTexCoord2f(0.0f, 0.0f); glVertex3f(-1.0f,  1.0f,  1.0f);
				glTexCoord2f(1.0f, 0.0f); glVertex3f( 1.0f,  1.0f,  1.0f);
				glTexCoord2f(1.0f, 1.0f); glVertex3f( 1.0f,  1.0f, -1.0f);
				// Bottom Face
				glNormal3f(0.0f, -1.0f, 0.0f);
				glTexCoord2f(1.0f, 1.0f); glVertex3f(-1.0f, -1.0f, -1.0f);
				glTexCoord2f(0.0f, 1.0f); glVertex3f( 1.0f, -1.0f, -1.0f);
				glTexCoord2f(0.0f, 0.0f); glVertex3f( 1.0f, -1.0f,  1.0f);
				glTexCoord2f(1.0f, 0.0f); glVertex3f(-1.0f, -1.0f,  1.0f);
				// Right face
				glNormal3f(1.0f, 0.0f, 0.0f);
				glTexCoord2f(1.0f, 0.0f); glVertex3f( 1.0f, -1.0f, -1.0f);
				glTexCoord2f(1.0f, 1.0f); glVertex3f( 1.0f,  1.0f, -1.0f);
				glTexCoord2f(0.0f, 1.0f); glVertex3f( 1.0f,  1.0f,  1.0f);
				glTexCoord2f(0.0f, 0.0f); glVertex3f( 1.0f, -1.0f,  1.0f);
				// Left Face
				glNormal3f(-1.0f, 0.0f, 0.0f);
				glTexCoord2f(0.0f, 0.0f); glVertex3f(-1.0f, -1.0f, -1.0f);
				glTexCoord2f(1.0f, 0.0f); glVertex3f(-1.0f, -1.0f,  1.0f);
				glTexCoord2f(1.0f, 1.0f); glVertex3f(-1.0f,  1.0f,  1.0f);
				glTexCoord2f(0.0f, 1.0f); glVertex3f(-1.0f,  1.0f, -1.0f);
				glEnd();*/
				
				this.DrawScene();
				
				//base.SwapBuffer();
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("TreeVizControl.glDraw(): " + ex.Message);
			}
		}
		
		protected override OpenGLContext CreateContext()
		{
			ControlGLContext context = new ControlGLContext(this);
			DisplayType displayType = new DisplayType(this.colorDepth, this.zDepth, this.stencilDepth, this.accumDepth);
			context.Create(displayType, null);
			return context;
		}

		protected override void OnSizeChanged(EventArgs e)
		{
			Size sz = this.Size;

			if (sz.Width != 0 && sz.Height != 0)
			{
				this.fHeight = sz.Height;
				this.fWidth = sz.Width;
				
				if (this.fHeight == 0) {
					this.fHeight = 1;
				}

				GL.glViewport(0, 0, this.fWidth, this.fHeight);
				GL.glMatrixMode(GL.GL_PROJECTION);
				GL.glLoadIdentity();
				
				GL.gluPerspective(this.fovY, (float)this.fWidth / (float)this.fHeight, this.nearClippingPlane, this.farClippingPlane);

				GL.glMatrixMode(GL.GL_MODELVIEW);
				GL.glLoadIdentity();
			}

			base.OnSizeChanged(e);
		}

		private uint RetrieveObjectID(int x, int y)
		{
			int objectsFound = 0;
			int[] viewportCoords = {0, 0, 0, 0};    // Массив для хранения экранных координат

			// Переменная для хранения ID обьектов, на которые мы кликнули.
			// Мы делаем массив в 32 элемента, т.к. OpenGL также сохраняет другую
			// информацию, которая нам сейчас не нужна. Для каждого обьекта нужно
			// 4 слота.
			uint[] selectBuffer = new uint[32];

			// glSelectBuffer регистрирует массив как буфер выбора обьектов. Первый параметр - размер
			// массива. Второй - сам массив для хранения информации.

			GL.glSelectBuffer(32, selectBuffer);   // Регистрируем буфер для хранения выбранных обьектов

			// Эта функция возвращает информацию о многих вещах в OpenGL. Мы передаём GL_VIEWPOR,
			// чтобы получить координаты экрана. Функция сохранит их в переданном вторым параметром массиве
			// в виде top,left,bottom,right.
			GL.glGetIntegerv(GL.GL_VIEWPORT, viewportCoords); // Получаем текущие координаты экрана

			// Теперь выходим из матрицы GL_MODELVIEW и переходим в матрицу GL_PROJECTION.
			// Это даёт возможность использовать X и Y координаты вместо 3D.

			GL.glMatrixMode(GL.GL_PROJECTION);    // Переходим в матрицу проекции
			GL.glPushMatrix();         // Переходим в новые экранные координаты

			// Эта функция делает так, что фреймбуфер не изменяется при рендере в него, вместо этого
			// происходит запись имён (ID) примитивов, которые были бы отрисованы при режиме
			// GL_RENDER. Информация помещается в selectBuffer.

			GL.glRenderMode(GL.GL_SELECT);    // Позволяет рендерить обьекты без изменения фреймбуфера
			GL.glLoadIdentity();       // Сбросим матрицу проекции

			// gluPickMatrix позволяет создавать матрицу проекции около нашего курсора. Проще говоря,
			// рендерится только область, которую мы укажем (вокруг курсора). Если обьект рендерится
			// в этой области, его ID сохраняется (Вот он, смысл всей функции).
			// Первые 2 параметра - X и Y координаты начала, следующие 2 - ширина и высота области
			// отрисовки. Последний параметр - экранные координаты. Заметьте, мы вычитаем 'y' из
			// НИЖНЕЙ экранной координаты. Мы сделали это, чтобы перевернуть Y координаты.
			// В 3д-пространстве нулевые y-координаты начинаются внизу, а в экранных координатах
			// 0 по y находится вверху. Также передаём регион 2 на 2 пиксела для поиска в нём обьекта.
			// Это может быть изменено как вам удобнее.
			GL.gluPickMatrix(x, viewportCoords[3] - y, 2, 2, viewportCoords);

			// Далее просто вызываем нашу нормальную функцию gluPerspective, точно так же, как
			// делали при инициализации.
			GL.gluPerspective(this.fovY, (float) fWidth / (float) fHeight, this.nearClippingPlane, this.farClippingPlane);

			GL.glMatrixMode(GL.GL_MODELVIEW); // Возвращаемся в матрицу GL_MODELVIEW

			this.glDraw();          // Теперь рендерим выбранную зону для выбора обьекта

			// Если мы вернёмся в нормальный режим рендеринга из режима выбора, glRenderMode
			// возвратит число обьектов, найденных в указанном регионе (в gluPickMatrix()).

			objectsFound = GL.glRenderMode(GL.GL_RENDER); // Вернемся в режим отрисовки и получим число обьектов

			GL.glMatrixMode(GL.GL_PROJECTION);    // Вернемся в привычную матрицу проекции
			GL.glPopMatrix();              // Выходим из матрицы
			GL.glMatrixMode(GL.GL_MODELVIEW);     // Вернемся в матрицу GL_MODELVIEW

			// Теперь нам нужно выяснить ID выбранных обьектов.
			// Если они есть - objectsFound должно быть как минимум 1.

			if (objectsFound > 0)
			{
				// Если мы нашли более 1 обьекта, нужно проверить значения глубины всех
				// выбоанных обьектов. Обьект с МЕНЬШИМ значением глубины - ближайший
				// к нам обьект, значит и щелкнули мы на него. В зависимости от того, что
				// мы программируем, нам могут понадобится и ВСЕ выбранные обьекты (если
				// некоторые были за ближайшим), но в этом уроке мы позаботимся только о
				// переднем обьекте. Итак, как нам получить значение глубины? Оно сохранено
				// в буфере выбора (selectionBuffer). Для каждого обьекта в нем 4 значения.
				// Первое - "число имен в массиве имен на момент события, далее минимум и
				// максимум значений глубины для всех вершин, которые были выбраны при прошлом
				// событии, далее по содержимое массива имен, нижнее имя - первое;
				// ("the number of names in the name stack at the time of the event, followed
				// by the minimum and maximum depth values of all vertices that hit since the
				// previous event, then followed by the name stack contents, bottom name first.") - MSDN.
				// Единстве, что нам нужно - минимальное значение глубины (второе значение) и
				// ID обьекта, переданного в glLoadName() (четвертое значение).
				// Итак, [0-3] - данные первого обьекта, [4-7] - второго, и т.д...
				// Будте осторожны, так как если вы отображаете на экране 2Д текст, он будет
				// всегда находится как ближайший обьект. Так что убедитесь, что отключили вывод
				// текста при рендеринге в режиме GL_SELECT. Я для этого использую флаг, передаваемый
				// в RenderScene(). Итак, получим обьект с минимальной глубиной!

				// При старте установим ближайшую глубину как глубину первого обьекта.
				// 1 - это минимальное Z-значение первого обьекта.
				uint lowestDepth = selectBuffer[1];

				// Установим выбранный обьект как первый при старте.
				// 3 - ID первого обьекта, переданный в glLoadName().
				uint selectedObject = selectBuffer[3];

				// Проходим через все найденные обьекты, начиная со второго (значения первого
				// мы присвоили изначально).
				for (int i = 1; i < objectsFound; i++)
				{
					// Проверяем, не ниже ли значение глубины текущего обьекта, чем предидущего.
					// Заметьте, мы умножаем i на 4 (4 значения на каждый обьект) и прибавляем 1 для глубины.
					if (selectBuffer[(i * 4) + 1] < lowestDepth)
					{
						// Установим новое низшее значение
						lowestDepth = selectBuffer[(i * 4) + 1];

						// Установим текущий ID обьекта
						selectedObject = selectBuffer[(i * 4) + 3];
					}
				}

				return selectedObject;
			}

			return 0;
		}

		private void Screenshot()
		{
			try {
				Image image = Context.ToImage();
				image.Save(@"d:\screenshot.jpg", ImageFormat.Jpeg);
				image.Dispose();
			}
			catch(Exception e) {
				MessageBox.Show(e.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Stop);
			}
		}

		protected override bool IsInputKey(Keys key)
		{
			switch(key) {
				case Keys.Up:
				case Keys.Down:
				case Keys.Right:
				case Keys.Left:
				case Keys.Tab:
					return true;
				default:
					return base.IsInputKey(key);
			}
		}

		protected override void OnKeyDown(KeyEventArgs e)
		{
			switch (e.KeyCode) {
				case Keys.F5: 
					this.Screenshot();
					break;

				case Keys.PageDown:
					z -= 0.5f;
					break;

				case Keys.PageUp:
					z += 0.5f;
					break;

				case Keys.R:
					/*if (e.Control)*/ this.FreeRotate = !this.FreeRotate;
					break;

				case Keys.D:
					/*if (e.Control)*/ this.Debug = !this.Debug;
					break;
					
				case Keys.T:
					this.TimeStop = !this.TimeStop;
					break;
			}
		}

		protected override void OnKeyUp(KeyEventArgs e)
		{
		}

		protected override void OnMouseDown(MouseEventArgs e)
		{
			base.OnMouseDown(e);

			if (!this.Focused) base.Focus();

			if (e.Button == MouseButtons.Left) {
				this.fMouseDrag = true;
				this.fLastX = e.X;
				this.fLastY = e.Y;
			}
		}

		protected override void OnMouseUp(MouseEventArgs e)
		{
			if (e.Button == MouseButtons.Left) {
				this.fMouseDrag = false;
			}
		}

		protected override void OnMouseMove(MouseEventArgs e)
		{
			if (this.fMouseDrag) {
				int dx = e.X - this.fLastX;
				int dy = e.Y - this.fLastY;

				if (this.fFreeRotate) {
					//xrot += 0.001f * dy;
					//yrot += 0.001f * dx;

					xrot += 0.001f * dy; //ok
					zrot += 0.001f * dx; //ok
				} else {
					zrot += 0.001f * dx;
				}
			} else {
				uint objectID = this.RetrieveObjectID(e.X, e.Y);

				switch (objectID) {
					case OBJ_X:
						SelectedObject = "X-axis";
						break;
					case OBJ_Y:
						SelectedObject = "Y-axis";
						break;
					case OBJ_Z:
						SelectedObject = "Z-axis";
						break;
					default:
						if (objectID >= OBJ_NODE) {
							int id = (int)(objectID - OBJ_NODE);

							TVPerson prs = this.FindPersonByIdx(id);
							if (prs != null) {
								SelectedObject = "["+prs.IRec.XRef+"] " + prs.IRec.GetNameString(true, false)+
									", " + prs.BirthYear.ToString() + " - " + prs.DeathYear.ToString();
							} else {
								SelectedObject = "<none>";
							}
						} else {
							SelectedObject = "<none>";
						}
						break;
				}
			}
		}

		protected override void OnMouseWheel(MouseEventArgs e)
		{
			if (e.Delta != 0) {
				z += 0.01f * e.Delta;
			}
		}

		#endregion

		#region Other
		
		/*private void LoadTextures() {
			string filename = @".\Crate.bmp";
			Bitmap bitmap = null;														// The Bitmap Image For Our Texture
			Rectangle rectangle;														// The Rectangle For Locking The Bitmap In Memory
			BitmapData bitmapData = null;												// The Bitmap's Pixel Data

			// Load The Bitmap
			try {
				bitmap = new Bitmap(filename);											// Load The File As A Bitmap
				bitmap.RotateFlip(RotateFlipType.RotateNoneFlipY);						// Flip The Bitmap Along The Y-Axis
				rectangle = new Rectangle(0, 0, bitmap.Width, bitmap.Height);			// Select The Whole Bitmap
				
				// Get The Pixel Data From The Locked Bitmap
				bitmapData = bitmap.LockBits(rectangle, ImageLockMode.ReadOnly, System.Drawing.Imaging.PixelFormat.Format24bppRgb);

				glGenTextures(3, texture);												// Create 3 Textures

				// Create Nearest Filtered Texture
				glBindTexture(GL_TEXTURE_2D, texture[0]);
				glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
				glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST); 
				glTexImage2D(GL_TEXTURE_2D, 0, (int) GL_RGB8, bitmap.Width, bitmap.Height, 0, GL_BGR_EXT, GL_UNSIGNED_BYTE, bitmapData.Scan0);

				// Create Linear Filtered Texture
				glBindTexture(GL_TEXTURE_2D, texture[1]);
				glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
				glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR); 
				glTexImage2D(GL_TEXTURE_2D, 0, (int) GL_RGB8, bitmap.Width, bitmap.Height, 0, GL_BGR_EXT, GL_UNSIGNED_BYTE, bitmapData.Scan0);

				// Create MipMapped Texture
				glBindTexture(GL_TEXTURE_2D, texture[2]);
				glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST);
				glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR); 
				gluBuild2DMipmaps(GL_TEXTURE_2D, (int) GL_RGB8, bitmap.Width, bitmap.Height, GL_BGR_EXT, GL_UNSIGNED_BYTE, bitmapData.Scan0);
			}
			catch(Exception e) {
				// Handle Any Exceptions While Loading Textures, Exit App
				string errorMsg = "An Error Occurred While Loading Texture:\n\t" + filename + "\n" + "\n\nStack Trace:\n\t" + e.StackTrace + "\n";
				MessageBox.Show(errorMsg, "Error", MessageBoxButtons.OK, MessageBoxIcon.Stop);
				App.Terminate();
			}
			finally {
				if(bitmap != null) {
					bitmap.UnlockBits(bitmapData);
					bitmap.Dispose();
				}
			}
		}*/

		#endregion

		#region TreeViz

        public void CreateArborGraph(IBase aBase, int minGens, bool loneSuppress)
        {
        	this.fBase = aBase;

        	try
        	{
        		fSys = new ArborSystem(1000, 1000, 0.1, null); //(10000, 1000, 0.1, this);
        		fSys.setScreenSize(50, 50);
        		fSys.OnStop += Arbor_OnStop;

        		using (ExtList<PatriarchObj> patList = new ExtList<PatriarchObj>(false)) {
        			aBase.Context.GetPatriarchsLinks(patList, minGens, false, loneSuppress);

        			int num = patList.Count;
        			for (int i = 0; i < num; i++) {
        				PatriarchObj p_obj = patList[i] as PatriarchObj;

        				if ((!loneSuppress) || (loneSuppress && p_obj.HasLinks)) {
        					ArborNode node = fSys.addNode(p_obj.IRec.XRef);
        					node.Data = p_obj;
        				}
        			}

        			for (int i = 0; i < num; i++) {
        				PatriarchObj pat1 = patList[i] as PatriarchObj;

        				int num2 = pat1.Links.Count;
        				for (int k = 0; k < num2; k++) {
        					PatriarchObj pat2 = pat1.Links[k];

        					fSys.addEdge(pat1.IRec.XRef, pat2.IRec.XRef, 1);
        				}
        			}
        		}

        		this.z = -50;

        		fSys.start();
        	}
        	catch (Exception ex)
        	{
        		SysUtils.LogWrite("TreeVizControl.CreateArborGraph(): " + ex.Message);
        	}
        }

		public void Arbor_OnStop(object sender, EventArgs eArgs)
		{
			this.FreeRotate = false;
			
			this.fMinYear = 0;

			try
			{
				// загрузить из ArborSystem точки и сигнатуры патриархов
				int num = fSys.Nodes.Count;
				for (int i = 0; i < num; i++)
				{
					ArborNode node = fSys.Nodes[i];
					GEDCOMIndividualRecord iRec = (GEDCOMIndividualRecord)fBase.Tree.XRefIndex_Find(node.Sign);
					int descGens = (node.Data as PatriarchObj).DescGenerations;

					TVPerson patr = this.PreparePerson(null, iRec);
					patr.Pt = new PointF((float)node.Pt.x * MAGIC_SCALE, (float)node.Pt.y * MAGIC_SCALE);
					patr.DescGenerations = descGens;
					//patr.IsPatriarch = true;
					patr.Type = TVPersonType.ptPatriarch;
					patr.BaseRadius = 100;

					if (this.fMinYear == 0) {
						this.fMinYear = patr.BirthYear;
					} else {
						if (this.fMinYear > patr.BirthYear) this.fMinYear = patr.BirthYear;
					}
				}

				// подготовить радиусы основания патриархов
				int num2 = fSys.Edges.Count;
				for (int i = 0; i < num2; i++) {
					ArborEdge edge = fSys.Edges[i];

					TVPerson srcPers = this.FindPersonByXRef(edge.Source.Sign);
					TVPerson tgtPers = this.FindPersonByXRef(edge.Target.Sign);

					float rad = (float)Dist(srcPers.Pt, tgtPers.Pt) * 3/7;

					if (srcPers.BaseRadius > rad) srcPers.BaseRadius = rad;
					if (tgtPers.BaseRadius > rad) tgtPers.BaseRadius = rad;
				}

				// подготовить диапазон лет
				int curYear = DateTime.Now.Year;
				this.fYearSize = BaseScale / (curYear - fMinYear);
				this.fTick = 0;
				this.fCurYear = this.fMinYear;

				// обработать дерево
				for (int i = 0, count = fPersons.Count; i < count; i++) {
					this.PrepareDescendants(fPersons[i]);
				}

				this.startTimer();
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("TreeVizControl.Arbor_OnStop(): " + ex.Message);
			}
		}

		private void PrepareDescendants(TVPerson person)
		{
			if (person == null) return;

			try
			{
				int gens = (person.DescGenerations <= 0) ? 1 : person.DescGenerations;
				person.GenSlice = person.BaseRadius / gens; // ?

				GEDCOMIndividualRecord iRec = person.IRec;

				if (iRec.SpouseToFamilyLinks.Count > 0)
				{
					PointF[] pts = GetCirclePoints(person.BeautySpouses, person.Pt, iRec.SpouseToFamilyLinks.Count, person.GenSlice / 3);

					int num2 = iRec.SpouseToFamilyLinks.Count;
					for (int k = 0; k < num2; k++)
					{
						GEDCOMFamilyRecord famRec = iRec.SpouseToFamilyLinks[k].Family;

						// обработать супруга текущей персоны
						GEDCOMIndividualRecord spouse = famRec.GetSpouseBy(iRec);
						if (spouse != null) {
							TVPerson sps = this.PreparePerson(null, spouse);
							sps.Pt = pts[k];
							person.Spouses.Add(sps);
						}

						// обработать детей текущей семьи
						int num3 = famRec.Childrens.Count;
						for (int m = 0; m < num3; m++)
						{
							GEDCOMIndividualRecord child = famRec.Childrens[m].Value as GEDCOMIndividualRecord;

							// исключить бездетные ветви
							if (EXCLUDE_CHILDLESS && (this.fBase.Context.IsChildless(child) || child.GetTotalChildsCount() < 1)) continue;

							TVPerson chp = this.PreparePerson(person, child);
							person.Childs.Add(chp);
						}
					}

					pts = GetCirclePoints(person.BeautyChilds, person.Pt, person.Childs.Count, person.BaseRadius / 2);

					int num = person.Childs.Count;
					for (int i = 0; i < num; i++)
					{
						TVPerson chp = person.Childs[i];
						chp.Pt = pts[i];
						chp.BaseRadius = (float)((person.BaseRadius / 2) * 0.95);
						chp.DescGenerations = person.DescGenerations - 1;
						
						this.PrepareDescendants(chp);
					}
				}
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("TreeVizControl.PrepareDescendants(): " + ex.Message);
			}
		}

		private void RecalcDescendants(TVPerson person)
		{
			// пересчитать координаты супругов, т.к. координаты данной персоны могли измениться
			PointF[] pts = GetCirclePoints(person.BeautySpouses, person.Pt, person.Spouses.Count, person.GenSlice / 3);
			for (int i = 0, count = person.Spouses.Count; i < count; i++)
			{
				person.Spouses[i].Pt = pts[i];
			}

			// счетчик детей, видимых на текущем шаге 
			int visCount = 0;
			for (int i = 0, count = person.Childs.Count; i < count; i++)
			{
				TVPerson chp = person.Childs[i];
				if (this.IsVisible(chp)) {
					visCount++;
				}
			}
			
			// пересчет координат видимых детей
			pts = GetCirclePoints(person.BeautyChilds, person.Pt, visCount, person.BaseRadius / 2);
			for (int i = 0, count = person.Childs.Count; i < count; i++)
			{
				TVPerson chp = person.Childs[i];

				if (this.IsVisible(chp)) {
					chp.Pt = pts[i];
					chp.BaseRadius = (float)((person.BaseRadius / 2) * 0.95);
					chp.DescGenerations = person.DescGenerations - 1;
					
					this.RecalcDescendants(chp);
				}
			}
		}

		private void RecalcDescendants()
		{
			return;
			for (int i = 0, count = fPersons.Count; i < count; i++)
			{
				TVPerson prs = fPersons[i];
				
				if (prs.Type == TVPersonType.ptPatriarch) {
					this.RecalcDescendants(prs);
				}
			}
		}
		
		// beauty - случайное смещение для "красоты", град
		private static PointF[] GetCirclePoints(int beauty, PointF center, int count, float radius)
		{
 			PointF[] result = new PointF[count];

 			if (count > 0)
 			{
 				// размер секции круга, град
 				float degSection = 360 / count;
 				
 				for (int i = 0; i < count; i++)
 				{
 					float degInRad = (beauty + i * degSection) * DEG2RAD;
 					float dx = (float)Math.Cos(degInRad) * radius;
 					float dy = (float)Math.Sin(degInRad) * radius;

 					result[i] = new PointF(center.X + dx, center.Y + dy);
 				}
 			}

 			return result;
		}

		/*private TVPerson TryPreparePerson(TVPerson parent, GEDCOMIndividualRecord iRec)
		{
			TVPerson result;
			
			if (this.fPersonsIndex.TryGetValue(iRec.XRef, result))
			{
				// добавляемая персона была уже ранее обработана при проходе от другого патриарха
				result.Parent = parent;
				// FIXME: добавить логику
				
				return null; // для того, чтобы вызывающий метод прекратил обработку
			} 
			else 
			{
				// добавляемая персона ранее не обрабатывалась
				return this.PreparePerson(parent, iRec);
			}
		}*/
		
		private TVPerson PreparePerson(TVPerson parent, GEDCOMIndividualRecord iRec)
		{
			TVPerson result;
			
			// FIXME: доработать
			//if (this.fPersonsIndex.TryGetValue(iRec.XRef, out result)) {
			//	return result;
			//} else {
				result = new TVPerson();
				result.Parent = parent;
				result.IRec = iRec;

				this.fPersons.Add(result);
				this.fPersonsIndex.Add(iRec.XRef, result);

				result.BirthYear = fBase.Context.FindBirthYear(iRec);
				result.DeathYear = fBase.Context.FindDeathYear(iRec);

				// FIXME переделать на предварительно статистически определенную продолжительность жизни.
				if (result.DeathYear <= 0) result.DeathYear = result.BirthYear + 75;
			//}

			return result;
		}

		private static double Dist(PointF pt1, PointF pt2)
		{
			double dx = pt2.X - pt1.X;
			double dy = pt2.Y - pt1.Y;
			return Math.Sqrt(dx * dx + dy * dy);
		}
		
		private TVPerson FindPersonByXRef(string xref)
		{
			for (int i = 0, count = fPersons.Count; i < count; i++)
			{
				TVPerson prs = fPersons[i];

				if (prs.IRec.XRef == xref) {
					return prs;
				}
			}

			return null;
		}
		
		private TVPerson FindPersonByIdx(int idx)
		{
			for (int i = 0, count = fPersons.Count; i < count; i++)
			{
				TVPerson prs = fPersons[i];

				if (prs.Idx == idx) {
					return prs;
				}
			}

			return null;
		}

		private void TV_Update(object sender, System.Timers.ElapsedEventArgs e)
		{
			if (this.fBusy) return;
			this.fBusy = true;
			try
			{
				//this.Redraw();
				
				if (!this.FreeRotate) {
					this.zrot -= 0.3f;
				}

				this.fTick += 1;

				if (!this.TimeStop && (this.fTick % 5 == 0)) {
					this.fCurYear += 1;
					this.RecalcDescendants();
				}
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("TreeVizControl.TV_Update(): " + ex.Message);
			}
			this.fBusy = false;
		}

		private void DrawScene()
		{
			try
			{
				DrawAxis();
				
				// FIXME: отметки выводятся от начала персонализированной шкалы времени,
				// нужно переделать на вывод по хронологическим векам
				// отметки веков
				for (int i = 0; i <= 5; i++) {
					GL.glPushMatrix();
					GL.glTranslatef(0, 0, i * 100 * this.fYearSize);
					GL.glColor3f(0.9F, 0.1F, 0.1F);
					DrawCircle(0.1F);
					GL.glPopMatrix();
				}

				this.DrawArborSystem();

				for (int i = 0, count = fPersons.Count; i < count; i++) {
					TVPerson prs = fPersons[i];
					this.DrawPerson(prs);
				}
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("TreeVizControl.DrawScene(): " + ex.Message);
			}
		}

		private void DrawArborSystem()
		{
			if (this.fSys == null) return;
			if (!this.Debug) return;

			try
			{
				int num = fSys.Nodes.Count;
				for (int i = 0; i < num; i++)
				{
					ArborNode node = fSys.Nodes[i];
					ArborPoint pt = node.Pt;

					GL.glPushMatrix();
					GL.glTranslatef((float)pt.x * MAGIC_SCALE, (float)pt.y * MAGIC_SCALE, 0);
					GL.glColor3f(0.9F, 0.3F, 0.3F);
					DrawCircle(0.1F);
					GL.glPopMatrix();
				}

				int num2 = fSys.Edges.Count;
				for (int i = 0; i < num2; i++)
				{
					ArborEdge edge = fSys.Edges[i];
					ArborPoint pt1 = edge.Source.Pt;
					ArborPoint pt2 = edge.Target.Pt;

					GL.glPushMatrix();
					GL.glColor3f(0.9F, 0.3F, 0.3F);
					GL.glBegin(GL.GL_LINES);
					GL.glVertex3f((float)pt1.x * MAGIC_SCALE, (float)pt1.y * MAGIC_SCALE, 0);
					GL.glVertex3f((float)pt2.x * MAGIC_SCALE, (float)pt2.y * MAGIC_SCALE, 0);
					GL.glEnd();
					GL.glPopMatrix();
				}
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("TreeVizControl.DrawArborSystem(): " + ex.Message);
			}
		}

		private bool IsVisible(TVPerson person)
		{
			if (person == null) return false;
			if (person.BirthYear > this.fCurYear) return false;

			if (person.BirthYear < this.fMinYear || person.DeathYear < this.fMinYear) {
				// персоны, для которых авто-определение дат не дало результата; не отображать
				return false;
				//string st = result.IRec.GetNameString(true, false);
				//Debug.WriteLine(st);
				//Debug.WriteLine(result.BirthYear.ToString() + " / " + result.DeathYear.ToString());
			}
			
			return true;
		}
		
		private void DrawPerson(TVPerson person)
		{
			if (person == null) return;
			if (!this.IsVisible(person)) return;

			try
			{
				int endYear = (this.fCurYear < person.DeathYear) ? this.fCurYear : person.DeathYear;

				float zBirth, zDeath;
				zBirth = this.fYearSize * (person.BirthYear - this.fMinYear);
				zDeath = this.fYearSize * (endYear - this.fMinYear);

				GEDCOMSex sex = person.IRec.Sex;
				PointF ppt = person.Pt;

				GL.glPushName(OBJ_NODE + (uint)person.Idx);
				GL.glPushMatrix();

				SetLineColor(sex);

				GL.glBegin(GL.GL_LINES);
				GL.glVertex3f(ppt.X, ppt.Y, zBirth);
				GL.glVertex3f(ppt.X, ppt.Y, zDeath);
				GL.glEnd();

				GL.glPopMatrix();
				GL.glPopName();

				if (this.Debug && person.Type == TVPersonType.ptPatriarch) {
					GL.glPushMatrix();
					GL.glTranslatef((float)ppt.X, (float)ppt.Y, 0);
					GL.glColor3f(0.9F, 0.1F, 0.1F);
					DrawCircle(person.BaseRadius);
					GL.glPopMatrix();
				}

				if (person.Parent != null) {
					PointF parentPt = person.Parent.Pt;

					GL.glPushMatrix();

					SetLineColor(sex);

					GL.glBegin(GL.GL_LINES);
					GL.glVertex3f(parentPt.X, parentPt.Y, zBirth);
					GL.glVertex3f(ppt.X, ppt.Y, zBirth);
					GL.glEnd();

					GL.glPopMatrix();
				}
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("TreeVizControl.DrawPerson(): " + ex.Message);
			}
		}

		private static void SetLineColor(GEDCOMSex sex)
		{
			switch (sex) {
				case GEDCOMSex.svMale:
					GL.glColor3f(0.1F, 0.3F, 0.9F);
					break;

				case GEDCOMSex.svFemale:
					GL.glColor3f(0.9F, 0.3F, 0.1F);
					break;
			}
		}

 		private static void DrawCircle(float radius)
		{
			GL.glBegin(GL.GL_LINE_LOOP);
			for (int i = 0; i <= 360; i++) {
				float degInRad = i * DEG2RAD;
				GL.glVertex2f((float)Math.Cos(degInRad) * radius, (float)Math.Sin(degInRad) * radius);
			}
			GL.glEnd();
		}

		private static void DrawAxis()
		{
			// draw z-axis
			GL.glPushName(OBJ_Z);
			GL.glColor3f(1.0F, 1.0F, 1.0F);
			GL.glBegin(GL.GL_LINES); // z
			GL.glVertex3f(0, 0, 0);
			GL.glVertex3f(0, 0, 100);
			GL.glEnd();
			GL.glColor3f(0.5F, 0.5F, 0.5F);
			GL.glBegin(GL.GL_LINES); // z
			GL.glVertex3f(0, 0, 0);
			GL.glVertex3f(0, 0, -100);
			GL.glEnd();
			GL.glPopName();
			
			// draw y-axis
			GL.glPushName(OBJ_Y);
			GL.glColor3f(0.0F, 0.0F, 1.0F);
			GL.glBegin(GL.GL_LINES); // y
			GL.glVertex3f(0, 0, 0);
			GL.glVertex3f(0, 100, 0);
			GL.glEnd();
			GL.glColor3f(0.0F, 0.0F, 0.5F);
			GL.glBegin(GL.GL_LINES); // y
			GL.glVertex3f(0, 0, 0);
			GL.glVertex3f(0, -100, 0);
			GL.glEnd();
			GL.glPopName();

			// draw x-axis
			GL.glPushName(OBJ_X);
			GL.glColor3f(0.0F, 1.0F, 0.0F);
			GL.glBegin(GL.GL_LINES); // x
			GL.glVertex3f(0, 0, 0);
			GL.glVertex3f(100, 0, 0);
			GL.glEnd();
			GL.glColor3f(0.0F, 0.5F, 0.0F);
			GL.glBegin(GL.GL_LINES); // x
			GL.glVertex3f(0, 0, 0);
			GL.glVertex3f(-100, 0, 0);
			GL.glEnd();
			GL.glPopName();
		}

		private void startTimer()
		{
			if (fAnimTimer != null) return;

			fAnimTimer = new System.Timers.Timer();
			fAnimTimer.AutoReset = true;
			fAnimTimer.Interval = 20; //50;
			fAnimTimer.Elapsed += this.TV_Update;
			fAnimTimer.Start();
		}

		private void stopTimer()
		{
			if (fAnimTimer == null) return;

			fAnimTimer.Stop();
			fAnimTimer = null;
		}

		#endregion
		
	}
}
