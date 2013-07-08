
#include <stdio.h>
#include <malloc.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>

#define	MEM_TRACKER_X		0
#define	MEM_TRACKER_Y		0
#define	MEM_TRACKER_W		(200)
#define	MEM_TRACKER_H		(100)
#define	MEM_TRACKER_B		0

int
mem_tracker (progname, memmax, count, timeout)

char *       progname;
int                    memmax;
int                            count;
double                                timeout;

{
#ifdef DEBUG
	static Display *display=NULL;
	static Window win=0;
	static Pixmap pixmap;
	static int screen;
	static Visual *visual;
	static Colormap cmap;
	static int depth;
	static GC gc;
	static GC gccl;
	unsigned long mask=0;
	XSetWindowAttributes watb;
	XColor background, foreground;
	static int bpixel, fpixel;
	XSizeHints size_hints;
	XEvent xevent;
	static int mem[MEM_TRACKER_W];
	static int nmem=0;
	int i, j1, j2;
	struct mallinfo malli;
	char title[256];
	double scale;

	if (display == NULL) {
		for (i=0; i<nmem; i++) mem[i] = 0;
		display = XOpenDisplay (NULL);
		if (display == NULL) {
			fprintf (stderr, "mem_tracker: Unable to connect to X server.\n");
			return (0);
		}
		screen = DefaultScreen (display);
		visual = DefaultVisual (display, screen);
		if (visual->class != PseudoColor) {
			fprintf (stderr, "mem_tracker: Can only do PseudoColor visual.\n");
			return (0);
		}
		cmap = DefaultColormap (display, screen);
		depth = DefaultDepth (display, screen);
		background.red   = 0x0000;
		background.green = 0x0000;
		background.blue  = 0xffff;
		background.flags = DoRed | DoGreen | DoBlue;
		XAllocColor (display, cmap, &background);
		foreground.red   = 0xffff;
		foreground.green = 0x0000;
		foreground.blue  = 0x0000;
		foreground.flags = DoRed | DoGreen | DoBlue;
		XAllocColor (display, cmap, &foreground);
		mask |= CWColormap;
		mask |= CWBackPixel;
		watb.colormap = cmap;
		watb.background_pixel = background.pixel;
		bpixel = background.pixel;
		fpixel = foreground.pixel;
		win = XCreateWindow(display, RootWindow(display, screen),
				MEM_TRACKER_X, MEM_TRACKER_Y,
				MEM_TRACKER_W, MEM_TRACKER_H,
				MEM_TRACKER_B, depth, InputOutput,
				visual, mask, &watb);
		if (win == 0) {
			fprintf (stderr, "mem_tracker: XCreateWindow() error.\n");
			return (0);
		}
		pixmap = XCreatePixmap (display, win, MEM_TRACKER_W, 
						MEM_TRACKER_H, depth);
		gc = XCreateGC (display, pixmap, NULL, NULL);
		XSetForeground (display, gc, fpixel);
		XSetBackground (display, gc, bpixel);
		gccl = XCreateGC (display, pixmap, NULL, NULL);
		XSetForeground (display, gccl, bpixel);
		XSetBackground (display, gccl, fpixel);
		sprintf (title, "%s: mem_tracker", progname);
		size_hints.flags = USPosition | USSize | PMinSize;
		size_hints.x = MEM_TRACKER_X;
		size_hints.y = MEM_TRACKER_Y;
		size_hints.width = MEM_TRACKER_W;
		size_hints.height = MEM_TRACKER_H;
		size_hints.min_width = MEM_TRACKER_W;
		size_hints.min_height = MEM_TRACKER_H;
		XSetStandardProperties(display, win, title, title,
				None, &progname, 1, &size_hints);
		XSelectInput(display, win, ExposureMask);
		XMapWindow(display, win);
		while (1) {
			XNextEvent(display, &xevent);
			if (xevent.type == Expose) break;
		}
	}
	XSync (display, True);
	XSelectInput(display, win, NULL);

	malli = mallinfo();
	if (nmem == MEM_TRACKER_W) {
		for (i=1; i<MEM_TRACKER_W; i++) mem[i-1] = mem[i];
		nmem--;
	}
	mem[nmem++] = malli.allocated;
	if (memmax <= 0) {
		memmax = 0;
		for (i=0; i<nmem; i++) {
			if (mem[i] > memmax) memmax = mem[i];
		}
	}
	scale = ((double) (MEM_TRACKER_H-1)) / ((double) memmax);
	XFillRectangle (display, pixmap, gccl, 0, 0, 
					MEM_TRACKER_W, MEM_TRACKER_H);
	for (i=0; i<nmem; i++) {
		j1 = (MEM_TRACKER_H-1) - (scale *  ((double) mem[i]));
		j2 = MEM_TRACKER_H-1;
		XDrawLine (display, pixmap, gc, i, j1, i, j2);
	}

	XCopyArea (display, pixmap, win, gc, 0, 0, MEM_TRACKER_W,
					MEM_TRACKER_H, 0, 0);
	XFlush (display);
	return (malli.allocated);
#else
	return 0;
#endif
}

/* $Id: mem_tracker.c,v 1.1 2001-06-15 00:18:30 kent Exp $ */
