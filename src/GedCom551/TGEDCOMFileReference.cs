using System;
using System.IO;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMFileReference : TGEDCOMTag
	{
		public TGEDCOMMultimediaFormat MultimediaFormat
		{
			get { return this.GetMultimediaFormat(); }
			set { this.SetMultimediaFormat(value); }
		}

		public TGEDCOMMediaType MediaType
		{
			get { return this.GetMediaType(); }
			set { this.SetMediaType(value); }
		}

		private TGEDCOMMultimediaFormat GetMultimediaFormat()
		{
			string S = base.GetTagStringValue("FORM").Trim().ToUpper();
			TGEDCOMMultimediaFormat Result;
			if (S == "")
			{
				Result = TGEDCOMMultimediaFormat.mfNone;
			}
			else
			{
				if (S == "BMP")
				{
					Result = TGEDCOMMultimediaFormat.mfBMP;
				}
				else
				{
					if (S == "GIF")
					{
						Result = TGEDCOMMultimediaFormat.mfGIF;
					}
					else
					{
						if (S == "JPG")
						{
							Result = TGEDCOMMultimediaFormat.mfJPG;
						}
						else
						{
							if (S == "OLE")
							{
								Result = TGEDCOMMultimediaFormat.mfOLE;
							}
							else
							{
								if (S == "PCX")
								{
									Result = TGEDCOMMultimediaFormat.mfPCX;
								}
								else
								{
									if (S == "TIF")
									{
										Result = TGEDCOMMultimediaFormat.mfTIF;
									}
									else
									{
										if (S == "WAV")
										{
											Result = TGEDCOMMultimediaFormat.mfWAV;
										}
										else
										{
											if (S == "TXT")
											{
												Result = TGEDCOMMultimediaFormat.mfTXT;
											}
											else
											{
												if (S == "RTF")
												{
													Result = TGEDCOMMultimediaFormat.mfRTF;
												}
												else
												{
													if (S == "AVI")
													{
														Result = TGEDCOMMultimediaFormat.mfAVI;
													}
													else
													{
														if (S == "TGA")
														{
															Result = TGEDCOMMultimediaFormat.mfTGA;
														}
														else
														{
															if (S == "PNG")
															{
																Result = TGEDCOMMultimediaFormat.mfPNG;
															}
															else
															{
																if (S == "MPG")
																{
																	Result = TGEDCOMMultimediaFormat.mfMPG;
																}
																else
																{
																	if (S == "HTM")
																	{
																		Result = TGEDCOMMultimediaFormat.mfHTM;
																	}
																	else
																	{
																		Result = TGEDCOMMultimediaFormat.mfUnknown;
																	}
																}
															}
														}
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
			return Result;
		}

		private void SetMultimediaFormat([In] TGEDCOMMultimediaFormat Value)
		{
			switch (Value)
			{
				case TGEDCOMMultimediaFormat.mfBMP:
				{
					base.SetTagStringValue("FORM", "bmp");
					return;
				}
				case TGEDCOMMultimediaFormat.mfGIF:
				{
					base.SetTagStringValue("FORM", "gif");
					return;
				}
				case TGEDCOMMultimediaFormat.mfJPG:
				{
					base.SetTagStringValue("FORM", "jpg");
					return;
				}
				case TGEDCOMMultimediaFormat.mfOLE:
				{
					base.SetTagStringValue("FORM", "ole");
					return;
				}
				case TGEDCOMMultimediaFormat.mfPCX:
				{
					base.SetTagStringValue("FORM", "pcx");
					return;
				}
				case TGEDCOMMultimediaFormat.mfTIF:
				{
					base.SetTagStringValue("FORM", "tif");
					return;
				}
				case TGEDCOMMultimediaFormat.mfWAV:
				{
					base.SetTagStringValue("FORM", "wav");
					return;
				}
				case TGEDCOMMultimediaFormat.mfTXT:
				{
					base.SetTagStringValue("FORM", "txt");
					return;
				}
				case TGEDCOMMultimediaFormat.mfRTF:
				{
					base.SetTagStringValue("FORM", "rtf");
					return;
				}
				case TGEDCOMMultimediaFormat.mfAVI:
				{
					base.SetTagStringValue("FORM", "avi");
					return;
				}
				case TGEDCOMMultimediaFormat.mfTGA:
				{
					base.SetTagStringValue("FORM", "tga");
					return;
				}
				case TGEDCOMMultimediaFormat.mfPNG:
				{
					base.SetTagStringValue("FORM", "png");
					return;
				}
				case TGEDCOMMultimediaFormat.mfMPG:
				{
					base.SetTagStringValue("FORM", "mpg");
					return;
				}
				case TGEDCOMMultimediaFormat.mfHTM:
				{
					base.SetTagStringValue("FORM", "htm");
					return;
				}
			}
			base.SetTagStringValue("FORM", "");
		}

		private TGEDCOMMediaType GetMediaType()
		{
			string S = base.GetTagStringValue(this.MediaTypeTagName()).Trim().ToUpper();
			TGEDCOMMediaType Result;
			if (S == "")
			{
				Result = TGEDCOMMediaType.mtNone;
			}
			else
			{
				if (S == "AUDIO")
				{
					Result = TGEDCOMMediaType.mtAudio;
				}
				else
				{
					if (S == "BOOK")
					{
						Result = TGEDCOMMediaType.mtBook;
					}
					else
					{
						if (S == "CARD")
						{
							Result = TGEDCOMMediaType.mtCard;
						}
						else
						{
							if (S == "ELECTRONIC")
							{
								Result = TGEDCOMMediaType.mtElectronic;
							}
							else
							{
								if (S == "FICHE")
								{
									Result = TGEDCOMMediaType.mtFiche;
								}
								else
								{
									if (S == "FILM")
									{
										Result = TGEDCOMMediaType.mtFilm;
									}
									else
									{
										if (S == "MAGAZINE")
										{
											Result = TGEDCOMMediaType.mtMagazine;
										}
										else
										{
											if (S == "MANUSCRIPT")
											{
												Result = TGEDCOMMediaType.mtManuscript;
											}
											else
											{
												if (S == "MAP")
												{
													Result = TGEDCOMMediaType.mtMap;
												}
												else
												{
													if (S == "NEWSPAPER")
													{
														Result = TGEDCOMMediaType.mtNewspaper;
													}
													else
													{
														if (S == "PHOTO")
														{
															Result = TGEDCOMMediaType.mtPhoto;
														}
														else
														{
															if (S == "TOMBSTONE")
															{
																Result = TGEDCOMMediaType.mtTombstone;
															}
															else
															{
																if (S == "VIDEO")
																{
																	Result = TGEDCOMMediaType.mtVideo;
																}
																else
																{
																	Result = TGEDCOMMediaType.mtUnknown;
																}
															}
														}
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
			return Result;
		}

		private void SetMediaType([In] TGEDCOMMediaType Value)
		{
			string S;
			switch (Value)
			{
				case TGEDCOMMediaType.mtAudio:
				{
					S = "audio";
					goto IL_AE;
				}
				case TGEDCOMMediaType.mtBook:
				{
					S = "book";
					goto IL_AE;
				}
				case TGEDCOMMediaType.mtCard:
				{
					S = "card";
					goto IL_AE;
				}
				case TGEDCOMMediaType.mtElectronic:
				{
					S = "electronic";
					goto IL_AE;
				}
				case TGEDCOMMediaType.mtFiche:
				{
					S = "fiche";
					goto IL_AE;
				}
				case TGEDCOMMediaType.mtFilm:
				{
					S = "film";
					goto IL_AE;
				}
				case TGEDCOMMediaType.mtMagazine:
				{
					S = "magazine";
					goto IL_AE;
				}
				case TGEDCOMMediaType.mtManuscript:
				{
					S = "manuscript";
					goto IL_AE;
				}
				case TGEDCOMMediaType.mtMap:
				{
					S = "map";
					goto IL_AE;
				}
				case TGEDCOMMediaType.mtNewspaper:
				{
					S = "newspaper";
					goto IL_AE;
				}
				case TGEDCOMMediaType.mtPhoto:
				{
					S = "photo";
					goto IL_AE;
				}
				case TGEDCOMMediaType.mtTombstone:
				{
					S = "tombstone";
					goto IL_AE;
				}
				case TGEDCOMMediaType.mtVideo:
				{
					S = "video";
					goto IL_AE;
				}
			}
			S = "";
			IL_AE:
			base.SetTagStringValue(this.MediaTypeTagName(), S);
		}

		protected override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FName = "FILE";
		}

		protected virtual string MediaTypeTagName()
		{
			return "FORM\\MEDI";
		}

		public void LinkFile([In] string AFile, TGEDCOMMediaType AMediaType, TGEDCOMMultimediaFormat AMultimediaFormat)
		{
			this.FStringValue = AFile;
			this.SetMultimediaFormat(TGEDCOMFileReference.RecognizeFormat(AFile));
			this.SetMediaType(AMediaType);
		}

		public static TGEDCOMMultimediaFormat RecognizeFormat([In] string AFile)
		{
			string E = Path.GetExtension(AFile).ToLower();
			TGEDCOMMultimediaFormat Result;
			if (E == ".bmp")
			{
				Result = TGEDCOMMultimediaFormat.mfBMP;
			}
			else
			{
				if (E == ".gif")
				{
					Result = TGEDCOMMultimediaFormat.mfGIF;
				}
				else
				{
					if (E == ".jpg" || E == ".jpeg")
					{
						Result = TGEDCOMMultimediaFormat.mfJPG;
					}
					else
					{
						if (E == ".ole")
						{
							Result = TGEDCOMMultimediaFormat.mfOLE;
						}
						else
						{
							if (E == ".pcx")
							{
								Result = TGEDCOMMultimediaFormat.mfPCX;
							}
							else
							{
								if (E == ".tif" || E == ".tiff")
								{
									Result = TGEDCOMMultimediaFormat.mfTIF;
								}
								else
								{
									if (E == ".wav")
									{
										Result = TGEDCOMMultimediaFormat.mfWAV;
									}
									else
									{
										if (E == ".txt")
										{
											Result = TGEDCOMMultimediaFormat.mfTXT;
										}
										else
										{
											if (E == ".rtf")
											{
												Result = TGEDCOMMultimediaFormat.mfRTF;
											}
											else
											{
												if (E == ".avi")
												{
													Result = TGEDCOMMultimediaFormat.mfAVI;
												}
												else
												{
													if (E == ".tga")
													{
														Result = TGEDCOMMultimediaFormat.mfTGA;
													}
													else
													{
														if (E == ".png")
														{
															Result = TGEDCOMMultimediaFormat.mfPNG;
														}
														else
														{
															if (E == ".mpg" || E == ".mpeg")
															{
																Result = TGEDCOMMultimediaFormat.mfMPG;
															}
															else
															{
																if (E == ".htm" || E == ".html")
																{
																	Result = TGEDCOMMultimediaFormat.mfHTM;
																}
																else
																{
																	Result = TGEDCOMMultimediaFormat.mfUnknown;
																}
															}
														}
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
			return Result;
		}

		public TGEDCOMFileReference(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
