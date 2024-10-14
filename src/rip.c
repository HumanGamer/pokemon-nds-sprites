/*
 * Copyright © 2011 magical
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <stdlib.h> /* EXIT_FAILURE, EXIT_SUCCESS, NULL, exit */
#include <stdio.h> /* FILE, fclose, fopen, fwrite, perror, printf, sprintf */
//#include <stdarg.h> /* va_list, va_end, va_start */
#include <string.h> /* strcat */
#include <limits.h> /* INT_MAX */

#ifdef _WIN32
# include <direct.h> /* _mkdir */
# define mkdir(path,mode)  _mkdir(path)
#else
# include <sys/stat.h> /* mkdir */
#endif

#include <errno.h> /* EEXIST, errno */

#include "common.h" /* FREE, ... */
#include "image.h"
#include "lzss.h"
#include "nitro.h"

#include "narc.h"
#include "ncgr.h"
#include "nclr.h"
#include "ncer.h"

#define MKDIR(dir) \
	if (mkdir(OUTDIR "/" dir, 0755)) { \
		switch (errno) { \
		case 0: \
		case EEXIST: \
			break; \
		default: \
			perror("mkdir: " OUTDIR "/" dir); \
			exit(EXIT_FAILURE); \
		} \
	}

/******************************************************************************/

static char magic_buf[MAGIC_BUF_SIZE];
#define STRMAGIC(magic) (strmagic((magic), magic_buf))

/******************************************************************************/

static struct NARC *
open_narc(const char *filename)
{
	assert(filename != NULL);

	FILE *fp = fopen(filename, "rb");
	if (fp == NULL) {
		goto error;
	}

	struct NARC *narc = nitro_read(fp, 0);

	if (narc == NULL) {
		goto error;
	}

	if (nitro_get_magic(narc) != (magic_t)'CRAN') {
		warn("Not a NARC");
		goto error;
	}

	return narc;

	error:
	if (errno) {
		perror("Failed to load NARC");
	} else {
		warn("Failed to load NARC");
	}
	exit(EXIT_FAILURE);
}

static void *
open_nitro(const char *filename, magic_t magic)
{
	char magicbuf[5];

	FILE *fp = fopen(filename, "rb");
	if (fp == NULL) {
		warn("Unable to open %s", filename);
		exit(EXIT_FAILURE);
	}

	void *file = nitro_read(fp, 0);
	if (file == NULL) {
		warn("Unable to read %s", filename);
		if (errno) {
			perror(NULL);
		}
		exit(EXIT_FAILURE);
	}
	if (*(magic_t *)file != magic) {
		warn("\"%s\" is not a %s", filename, strmagic(magic, magicbuf));
		exit(EXIT_FAILURE);
	}

	//no fclose()
	return file;
}

/******************************************************************************/

static int
list(void)
{
	#define FILENAME "./Resources/Narcs/pokegra.narc"
	#define OUTDIR "./Out/test"
	struct NARC *narc;
	struct nitro *chunk;

	narc = open_narc(FILENAME);

	u32 count = narc_get_file_count(narc);
	assert(count <= INT_MAX);
	for (int i = 0; i < (int)count; i++) {
		u32 size = narc_get_file_size(narc, i);
		if (0 < size) {
			chunk = narc_load_file(narc, i);
			if (chunk != NULL) {
				printf("%3d %s\n", i, STRMAGIC(nitro_get_magic(chunk)));
			} else {
				printf("%3d (error)\n", i);
			}
		} else {
			printf("%3d (empty)\n", i);
		}
	}

	exit(EXIT_SUCCESS);
}


static void
write_sprite(struct image *image, char *outfile)
{
	strcat(outfile, ".png");
	FILE *outfp = fopen(outfile, "wb");
	if (outfp != NULL) {
		if (image_write_png(image, outfp)) {
			warn("Error writing %s.", outfile);
		}
		fclose(outfp);
	} else {
		perror(outfile);
	}
}

static void
rip_sprites(void)
{
	#define FILENAME "./Resources/Narcs/pokegra.narc"
	#define OUTDIR "./Out/test"
	struct NARC *narc = open_narc(FILENAME);
	char outfile[256] = "";

	const struct sprite_dirs {
		const char *normal;
		const char *shiny;
	} dirs[] = {
		{"back/female", "back/shiny/female"},
		{"back", "back/shiny"},
		{"female", "shiny/female"},
		{"", "shiny"},
	};
	MKDIR("")
	MKDIR("female")
	MKDIR("shiny")
	MKDIR("shiny/female")
	MKDIR("back")
	MKDIR("back/female")
	MKDIR("back/shiny")
	MKDIR("back/shiny/female")

	struct image image = {};

	for (int n = 1; n <= 493; n++) {
		struct NCLR *normal_nclr = narc_load_file(narc, n*6 + 4);
		struct NCLR *shiny_nclr = narc_load_file(narc, n*6 + 5);

		if (normal_nclr == NULL || shiny_nclr == NULL) {
			if (errno) perror(NULL);
			else warn("Error reading palettes.");
			exit(EXIT_FAILURE);
		}

		assert(nitro_get_magic(normal_nclr) == (magic_t)'NCLR');
		assert(nitro_get_magic(shiny_nclr) == (magic_t)'NCLR');

		struct palette *normal_palette = nclr_get_palette(normal_nclr, 0);
		struct palette *shiny_palette = nclr_get_palette(shiny_nclr, 0);

		nitro_free(normal_nclr);
		nitro_free(shiny_nclr);

		FREE(normal_nclr);
		FREE(shiny_nclr);

		if (normal_palette == NULL || shiny_palette == NULL) {
			if (errno) perror(NULL);
			else warn("Error loading palettes.");
			exit(EXIT_FAILURE);
		}

		for (int i = 0; i < 4; i++) {
			const struct sprite_dirs *d = &dirs[i];

			struct NCGR *ncgr = narc_load_file(narc, n*6 + i);
			if (ncgr == NULL) {
				// this is fine
				continue;
			}

			assert(nitro_get_magic(ncgr) == (magic_t)'NCGR');
			ncgr_decrypt_pt(ncgr);

			sprintf(outfile, "%s/%s/%d", OUTDIR, d->normal, n);

			image.pixels = ncgr_get_pixels(ncgr);
			if (image.pixels == NULL) {
				warn("Error ripping %s.", outfile);
				continue;
			}

			ncgr_get_dim(ncgr, &image.dim);

			nitro_free(ncgr);
			FREE(ncgr);

			image.palette = normal_palette;
			write_sprite(&image, outfile);

			sprintf(outfile, "%s/%s/%d", OUTDIR, d->shiny, n);
			image.palette = shiny_palette;
			write_sprite(&image, outfile);

			FREE(image.pixels);
		}

		FREE(normal_palette->colors);
		FREE(shiny_palette->colors);

		FREE(normal_palette);
		FREE(shiny_palette);
	}

	printf("done\n");
	exit(EXIT_SUCCESS);
}

static void
rip_bw_sprites(void)
{
	#define FILENAME "./Resources/Narcs/pokegra-w.narc"
	#define OUTDIR "./Out/Sprites"
	struct NARC *narc = open_narc(FILENAME);
	struct NCER *ncer = open_nitro("./Resources/bw-pokemon.ncer", 'NCER');

	char outfile[256] = "";

	const struct sprite_dirs {
		const char *normal;
		const char *shiny;
	} dirs[] = {
		{"", "shiny"},
		{"female", "shiny/female"},
		{"back", "back/shiny"},
		{"back/female", "back/shiny/female"},
	};

	MKDIR("")
	MKDIR("female")
	MKDIR("shiny")
	MKDIR("shiny/female")
	MKDIR("back")
	MKDIR("back/female")
	MKDIR("back/shiny")
	MKDIR("back/shiny/female")

	struct image image = {};

	for (int n = 0; n <= 711; n++) {
		struct NCLR *normal_nclr = narc_load_file(narc, n*20 + 18);
		struct NCLR *shiny_nclr = narc_load_file(narc, n*20 + 19);

		if (normal_nclr == NULL || shiny_nclr == NULL) {
			if (errno) perror(NULL);
			else warn("Error reading palettes.");
			exit(EXIT_FAILURE);
		}

		assert(nitro_get_magic(normal_nclr) == (magic_t)'NCLR');
		assert(nitro_get_magic(shiny_nclr) == (magic_t)'NCLR');

		struct palette *normal_palette = nclr_get_palette(normal_nclr, 0);
		struct palette *shiny_palette = nclr_get_palette(shiny_nclr, 0);

		nitro_free(normal_nclr);
		nitro_free(shiny_nclr);

		FREE(normal_nclr);
		FREE(shiny_nclr);

		if (normal_palette == NULL || shiny_palette == NULL) {
			if (errno) perror(NULL);
			else warn("Error loading palettes.");
			exit(EXIT_FAILURE);
		}

		for (int i = 0; i < 4; i++) {
			const struct sprite_dirs *d = &dirs[i];

			struct NCGR *ncgr;
			int index = 0;
			switch (i) {
			case 0: case 1: index = n * 20 + i; break;
			case 2: case 3: index = n * 20 + 9 + (i - 2); break;
			}
			if (narc_get_file_size(narc, index) == 0) {
				// this is fine
				continue;
			}
			ncgr = narc_load_file(narc, index);
			if (ncgr == NULL) {
				warn("error getting file %d", index);
				continue;
			}

			assert(nitro_get_magic(ncgr) == (magic_t)'NCGR');

			sprintf(outfile, "%s/%s/%d", OUTDIR, d->normal, n);

			ncgr_get_dim(ncgr, &image.dim);

			image.pixels = buffer_alloc(image.dim.height * image.dim.width);
			if (image.pixels == NULL) {
				warn("Error ripping %s.", outfile);
				continue;
			}

			struct coords offset = {0,0};
			if (ncer_draw_cell(ncer, 0, ncgr, &image, offset)) {
				warn("error drawing cell");
			}
			/* if (ncer_draw_boxes(ncer, 0, &image, offset)) {
				warn("error drawing boxes");
			} */

			nitro_free(ncgr);
			FREE(ncgr);

			image.palette = normal_palette;
			write_sprite(&image, outfile);

			sprintf(outfile, "%s/%s/%d", OUTDIR, d->shiny, n);
			image.palette = shiny_palette;
			write_sprite(&image, outfile);

			FREE(image.pixels);
		}

		FREE(normal_palette->colors);
		FREE(shiny_palette->colors);

		FREE(normal_palette);
		FREE(shiny_palette);
	}

	printf("done\n");
	exit(EXIT_SUCCESS);
}

static void
rip_bw_beta_sprites(void)
{
	#define FILENAME "./Resources/Narcs/pokegra-w.narc"
	#define OUTDIR "./Out/Sprites"
	struct NARC *narc = open_narc(FILENAME);
	struct NCER *ncer = open_nitro("./Resources/bw-pokemon.ncer", 'NCER');

	char outfile[256] = "";

	const struct sprite_dirs {
		const char *normal;
		const char *shiny;
	} dirs[] = {
		{"", "shiny"},
		{"female", "shiny/female"},
		{"back", "back/shiny"},
		{"back/female", "back/shiny/female"},
	};

	MKDIR("")
	MKDIR("female")
	MKDIR("shiny")
	MKDIR("shiny/female")
	MKDIR("back")
	MKDIR("back/female")
	MKDIR("back/shiny")
	MKDIR("back/shiny/female")

	struct image image = {};

	for (int n = 0; n <= 711; n++) {
		struct NCLR *normal_nclr = narc_load_file(narc, n*20 + 18);
		struct NCLR *shiny_nclr = narc_load_file(narc, n*20 + 19);

		if (normal_nclr == NULL || shiny_nclr == NULL) {
			if (errno) perror(NULL);
			else warn("Error reading palettes.");
			exit(EXIT_FAILURE);
		}

		assert(nitro_get_magic(normal_nclr) == (magic_t)'NCLR');
		assert(nitro_get_magic(shiny_nclr) == (magic_t)'NCLR');

		struct palette *normal_palette = nclr_get_palette(normal_nclr, 0);
		struct palette *shiny_palette = nclr_get_palette(shiny_nclr, 0);

		nitro_free(normal_nclr);
		nitro_free(shiny_nclr);

		FREE(normal_nclr);
		FREE(shiny_nclr);

		if (normal_palette == NULL || shiny_palette == NULL) {
			if (errno) perror(NULL);
			else warn("Error loading palettes.");
			exit(EXIT_FAILURE);
		}

		for (int i = 0; i < 4; i++) {
			const struct sprite_dirs *d = &dirs[i];

			struct NCGR *ncgr;
			int index = 0;
			switch (i) {
			case 0: case 1: index = n * 20 + i; break;
			case 2: case 3: index = n * 20 + 9 + (i - 2); break;
			}
			if (narc_get_file_size(narc, index) == 0) {
				// this is fine
				continue;
			}
			ncgr = narc_load_file(narc, index);
			if (ncgr == NULL) {
				warn("error getting file %d", index);
				continue;
			}

			assert(nitro_get_magic(ncgr) == (magic_t)'NCGR');

			sprintf(outfile, "%s/%s/%d", OUTDIR, d->normal, n);

			ncgr_get_dim(ncgr, &image.dim);

			image.pixels = ncgr_get_pixels(ncgr);//buffer_alloc(image.dim.height * image.dim.width);
			if (image.pixels == NULL) {
				warn("Error ripping %s.", outfile);
				continue;
			}

			struct coords offset = {0,0};
			//if (ncer_draw_cell(ncer, 0, ncgr, &image, offset)) {
			//	warn("error drawing cell");
			//}
			/* if (ncer_draw_boxes(ncer, 0, &image, offset)) {
				warn("error drawing boxes");
			} */

			nitro_free(ncgr);
			FREE(ncgr);

			image.palette = normal_palette;
			write_sprite(&image, outfile);

			sprintf(outfile, "%s/%s/%d", OUTDIR, d->shiny, n);
			image.palette = shiny_palette;
			write_sprite(&image, outfile);

			FREE(image.pixels);
		}

		FREE(normal_palette->colors);
		FREE(shiny_palette->colors);

		FREE(normal_palette);
		FREE(shiny_palette);
	}

	printf("done\n");
	exit(EXIT_SUCCESS);
}

static void
rip_bw_trainers(void)
{
	#define OUTDIR "./Out/Trainers"
	#define FILENAME "./Resources/Narcs/trfgra.narc"
	struct NARC *narc = open_narc(FILENAME);
	struct NCER *ncer = open_nitro("./Resources/bw-trainer.ncer", 'NCER');

	char outfile[256] = "";

	MKDIR("")
	MKDIR("parts")

	struct image image = {};

	for (int n = 0; n <= 187; n++) {
		printf("%d\n", n);
		struct NCLR *normal_nclr = narc_load_file(narc, n*8 + 7);

		if (normal_nclr == NULL) {
			if (errno) perror(NULL);
			else warn("Error reading palettes.");
			exit(EXIT_FAILURE);
		}

		assert(nitro_get_magic(normal_nclr) == (magic_t)'NCLR');

		struct palette *normal_palette = nclr_get_palette(normal_nclr, 0);

		nitro_free(normal_nclr);

		FREE(normal_nclr);

		if (normal_palette == NULL) {
			if (errno) perror(NULL);
			else warn("Error loading palettes.");
			exit(EXIT_FAILURE);
		}

		for (int i = 0; i < 2; i++) {
			const char *dir;

			switch (i) {
			case 0: dir = ""; break;
			case 1: dir = "parts"; break;
			}

			struct NCGR *ncgr;
			int index = n * 8 + i;

			if (narc_get_file_size(narc, index) == 0) {
				// this is fine
				continue;
			}
			ncgr = narc_load_file(narc, index);
			if (ncgr == NULL) {
				warn("error getting file %d", index);
				continue;
			}

			assert(nitro_get_magic(ncgr) == (magic_t)'NCGR');

			sprintf(outfile, "%s/%s/%d", OUTDIR, dir, n);

			ncgr_get_dim(ncgr, &image.dim);

			if (i == 0) {
				image.pixels = buffer_alloc(image.dim.height * image.dim.width);
				if (image.pixels == NULL) {
					warn("Error ripping %s.", outfile);
					continue;
				}
				struct coords offset = {0,0};
				if (ncer_draw_cell(ncer, 0, ncgr, &image, offset)) {
					warn("error drawing cell");
				}
				/* if (ncer_draw_boxes(ncer, 0, &image, offset)) {
					warn("error drawing boxes");
				} */
			} else if (i == 1) {
				image.pixels = ncgr_get_pixels(ncgr);
				if (image.pixels == NULL) {
					warn("Error ripping %s.", outfile);
					continue;
				}
			}


			nitro_free(ncgr);
			FREE(ncgr);

			image.palette = normal_palette;
			write_sprite(&image, outfile);

			FREE(image.pixels);
		}

		FREE(normal_palette->colors);

		FREE(normal_palette);
	}

	printf("done\n");
	exit(EXIT_SUCCESS);
}

/* for d/p */
static void
rip_trainers(void)
{
	#define OUTDIR "./Out/test"
	#define FILENAME "./Resources/Narcs/trfgra.narc"
	struct NARC *narc = open_narc(FILENAME);
	char outfile[256] = "";
	const int trainer_count = narc_get_file_count(narc) / 2;

	struct image image = {};

	MKDIR("");

	for (int n = 0; n < trainer_count; n++) {
		sprintf(outfile, "%s/%d", OUTDIR, n);

		struct NCGR *ncgr = narc_load_file(narc, n*2 + 0);
		if (ncgr == NULL) {
			if (errno) perror(outfile);
			continue;
		}
		assert(nitro_get_magic(ncgr) == (magic_t)'NCGR');

		struct NCLR *nclr = narc_load_file(narc, n*2 + 1);
		if (nclr == NULL) {
			if (errno) perror(outfile);
			nitro_free(ncgr);
			FREE(ncgr);
			continue;
		}
		assert(nitro_get_magic(nclr) == (magic_t)'NCLR');

		image.palette = nclr_get_palette(nclr, 0);
		if (image.palette == NULL) {
			warn("Error ripping %s.", outfile);
			continue; // leak
		}

		nitro_free(nclr);
		FREE(nclr);

		ncgr_decrypt_pt(ncgr);

		image.pixels = ncgr_get_pixels(ncgr);
		if (image.pixels == NULL) {
			warn("Error ripping %s.", outfile);
			continue; // leak
		}

		ncgr_get_dim(ncgr, &image.dim);

		nitro_free(ncgr);
		FREE(ncgr);

		write_sprite(&image, outfile);

		FREE(image.pixels);
		FREE(image.palette->colors);
		FREE(image.palette);
	}

	printf("done\n");
	exit(EXIT_SUCCESS);
}

/* for hg/ss */
static void
rip_trainers2(void)
{
	#define FILENAME "./Resources/Narcs/trbgra.narc"
	#define OUTDIR "./Out/test"
	struct NARC *narc = open_narc(FILENAME);
	char outfile[256] = "";
	const int trainer_count = narc_get_file_count(narc) / 5;

	MKDIR("frames");

	struct image image = {};

	for (int n = 0; n < trainer_count; n++) {
		struct NCLR *nclr = narc_load_file(narc, n*5 + 1);
		if (nclr == NULL) {
			if (errno) perror(NULL);
			continue;
		}
		assert(nitro_get_magic(nclr) == (magic_t)'NCLR');

		image.palette = nclr_get_palette(nclr, 0);
		if (image.palette == NULL) {
			if (errno) perror(NULL);
			nitro_free(nclr);
			FREE(nclr);
			continue;
		}

		nitro_free(nclr);
		FREE(nclr);

		int spriteindex;
		for (int i = 0; i < 2; i++) {
			switch (i) {
			case 0:
				spriteindex = 0;
				sprintf(outfile, "%s/frames/%d", OUTDIR, n);
				break;
			case 1:
				spriteindex = 4;
				sprintf(outfile, "%s/%d", OUTDIR, n);
				break;
			}
			puts(outfile);

			struct NCGR *ncgr = narc_load_file(narc, n*5 + spriteindex);
			if (ncgr == NULL) {
				if (errno) perror(outfile);
				continue;
			}
			assert(nitro_get_magic(ncgr) == (magic_t)'NCGR');

			if (i == 1) {
				/* pt for platinum, dp for hgss */
				ncgr_decrypt_dp(ncgr);
			}

			image.pixels = ncgr_get_pixels(ncgr);
			if (image.pixels == NULL) {
				warn("Error ripping %s.", outfile);
				nitro_free(ncgr);
				FREE(ncgr);
				continue;
			}

			ncgr_get_dim(ncgr, &image.dim);

			nitro_free(ncgr);
			FREE(ncgr);

			write_sprite(&image, outfile);

			FREE(image.pixels);
		}
		FREE(image.palette->colors);
		FREE(image.palette);
	}

	printf("done\n");
	exit(EXIT_SUCCESS);
}

/* for d/p/pt */
static void
rip_textbox_dppt(void)
{
	#define OUTDIR "./Out/winFrame"
	#define FILENAME "./Resources/Narcs/winframe.narc"
	struct NARC *narc = open_narc(FILENAME);
	char outfile[256] = "";

	struct image image = {};

	MKDIR("");

	for (int n = 2; n <= 21; n++) {
		sprintf(outfile, "%s/%d", OUTDIR, n-2);

		struct NCGR *ncgr = narc_load_file(narc, n);
		assert(nitro_get_magic(ncgr) == (magic_t)'NCGR');

		struct NCLR *nclr = narc_load_file(narc, n+23);
		assert(nitro_get_magic(nclr) == (magic_t)'NCLR');

		image.palette = nclr_get_palette(nclr, 0);	

		nitro_free(nclr);
		FREE(nclr);

		image.pixels = ncgr_get_pixels(ncgr);
		if (image.pixels == NULL) {
			warn("Error ripping %s.", outfile);
			continue; // leak
		}

		ncgr_get_dim(ncgr, &image.dim);

		nitro_free(ncgr);
		FREE(ncgr);

		write_sprite(&image, outfile);

		FREE(image.pixels);
		FREE(image.palette->colors);
		FREE(image.palette);
	}

	printf("done\n");
	exit(EXIT_SUCCESS);
}

/* for hgss */
static void
rip_textbox_hgss(void)
{
	#define OUTDIR "./Out/winFrame"
	#define FILENAME "./Resources/Narcs/winframe.narc"
	struct NARC *narc = open_narc(FILENAME);
	char outfile[256] = "";

	struct image image = {};

	MKDIR("");

	for (int n = 2; n <= 21; n++) {
		sprintf(outfile, "%s/%d", OUTDIR, n-2);

		struct NCGR *ncgr = narc_load_file(narc, n);
		assert(nitro_get_magic(ncgr) == (magic_t)'NCGR');

		struct NCLR *nclr = narc_load_file(narc, n+24);
		assert(nitro_get_magic(nclr) == (magic_t)'NCLR');

		image.palette = nclr_get_palette(nclr, 0);	

		nitro_free(nclr);
		FREE(nclr);

		image.pixels = ncgr_get_pixels(ncgr);
		if (image.pixels == NULL) {
			warn("Error ripping %s.", outfile);
			continue; // leak
		}

		ncgr_get_dim(ncgr, &image.dim);

		nitro_free(ncgr);
		FREE(ncgr);

		write_sprite(&image, outfile);

		FREE(image.pixels);
		FREE(image.palette->colors);
		FREE(image.palette);
	}

	printf("done\n");
	exit(EXIT_SUCCESS);
}

static void
rip_footprint(void)
{
	#define FILENAME "./Resources/Narcs/pokefoot.narc"
	#define OUTDIR "./Out/Footprints"
	MKDIR("");

	struct NARC *narc = open_narc(FILENAME);

	struct NCER *ncer = narc_load_file(narc, 2);
	assert(nitro_get_magic(ncer) == (magic_t)'NCER');

	struct NCLR *nclr = narc_load_file(narc, 0);
	assert(nitro_get_magic(nclr) == (magic_t)'NCLR');

	for (size_t i = 3; i < narc_get_file_count(narc); i++){

		char outfile[256] = "";
		sprintf(outfile, "%s/%d", OUTDIR, i-3);

		struct NCGR *ncgr = narc_load_file(narc, i);
		assert(nitro_get_magic(ncgr) == (magic_t)'NCGR');

		ncer_dump(ncer, NULL);

		struct dim dim;
		ncgr_get_dim(ncgr, &dim);

		warn("ncer.dim = {.width=%u, .height=%u}", dim.width, dim.height);

		struct image image = {
			.palette = nclr_get_palette(nclr, 0),
			.pixels = buffer_alloc(16*16),
			.dim = {16,16},
		};

		struct coords offset = {8, 8};
		ncer_draw_cell(ncer, 0, ncgr, &image, offset);

		write_sprite(&image, outfile);
	}

	

	exit(EXIT_SUCCESS);
}

static void
rip_trainer(void)
{
	#define FILENAME "./Resources/Narcs/trfgra.narc"
	#define FILENAME2 "./Resources/Narcs/trbgra.narc"
	#define OUTDIR "./Out/Trainers"
	#define OUTDIR2 "./Out/Trainers/Back"
	MKDIR("");
	MKDIR("Back");

	for (int time = 0; time < 2; time++){
		struct NARC *narc = open_narc(FILENAME);
		if (time == 1){
			narc = open_narc(FILENAME2);
		}
		int trainer_count = narc_get_file_count(narc) / 5;
		
		for (int i = 0; i < trainer_count; i++){

			struct NCER *ncer = narc_load_file(narc, i * 5 + 2);
			assert(nitro_get_magic(ncer) == (magic_t)'NCER');

			struct NCLR *nclr = narc_load_file(narc, i * 5 + 1);
			assert(nitro_get_magic(nclr) == (magic_t)'NCLR');

			struct NCGR *ncgr = narc_load_file(narc, i * 5);
			assert(nitro_get_magic(ncgr) == (magic_t)'NCGR');

			char outfile[256] = "";
			if (time == 1){
				sprintf(outfile, "%s/%d", OUTDIR2, i);
			}else{
				sprintf(outfile, "%s/%d", OUTDIR, i);
			}

			//ncer_dump(ncer, NULL);

			struct dim dim;
			ncgr_get_dim(ncgr, &dim);

			//warn("ncer.dim = {.width=%u, .height=%u}", dim.width, dim.height);

			const int frames = ncer_get_cell_count(ncer);
			const int size = 80*frames;
			int size2 = 80;
			if (time == 1){
				size2 = 128;
			}

			struct image image = {
				.palette = nclr_get_palette(nclr, 0),
				.pixels = buffer_alloc(size2*size),
				.dim = {size,size2},
			};
			
			for (int t = 1; t <= frames; t++){
				if (i == 106 && t == 1 && time == 0){
					size2 = 112;
				}else if(i == 106 && t != 1){
					size2 = 80;
				}else if(time == 0){
					size2 = 80;
				}else if(time == 1){
					size2 = 128;
				}
				//Trainer 106 in HGSS doesn't properly show the 1st frame as it's cut off due to the anim - just move it forward 16 px
				//DPPt has only 104 trainers so it will never be "wrong" for DPPt
				struct coords offset = {size2/2, (80*t) - 40};
				ncer_draw_cell(ncer, t-1, ncgr, &image, offset);
			}


			write_sprite(&image, outfile);
		}
	}
	

	

	exit(EXIT_SUCCESS);
}

static void
rip_icon(void)
{

	MKDIR("");

	#define FILENAME "./Resources/Narcs/poke_icon.narc"
	#define OUTDIR "./Out/test"
	struct NARC *narc = open_narc(FILENAME);

	struct NCER *ncer = narc_load_file(narc, 4);
	//assert(nitro_get_magic(ncer) == (magic_t)'NCER');
	
	struct NCLR *nclr = narc_load_file(narc, 0);
	//assert(nitro_get_magic(nclr) == (magic_t)'NCLR');
	for (size_t i = 5; i < narc_get_file_count(narc); i++)
	{
		char outfile[256] = "";
		sprintf(outfile, "%s/%d", OUTDIR, i-5);
		struct NCGR *ncgr = narc_load_file(narc, i);
		//assert(nitro_get_magic(ncgr) == (magic_t)'NCGR');

		ncer_dump(ncer, NULL);

		struct dim dim;
		ncgr_get_dim(ncgr, &dim);

		warn("ncer.dim = {.width=%u, .height=%u}", dim.width, dim.height);

		struct image image = {
			.palette = nclr_get_palette(nclr, 0),
			.pixels = buffer_alloc(32*24),
			.dim = {24,32},
		};

		struct coords offset = {16, 8};
		ncer_draw_cell(ncer, 0, ncgr, &image, offset);

		
		write_sprite(&image, outfile);
		
		nitro_free(ncgr);
		FREE(ncgr);

	}
	nitro_free(ncer);
	FREE(ncer);
	nitro_free(nclr);
	FREE(nclr);

	exit(EXIT_SUCCESS);
}

static void
bwrip_icon(void)
{
	#define FILENAME "./Resources/Narcs/poke_icon-w.narc"
	#define OUTDIR "./Out/pokeIcons"

	MKDIR("");

	struct NARC *narc = open_narc(FILENAME);

	struct NCER *ncer = narc_load_file(narc, 2);
	//assert(nitro_get_magic(ncer) == (magic_t)'NCER');
	
	struct NCLR *nclr = narc_load_file(narc, 0);
	//assert(nitro_get_magic(nclr) == (magic_t)'NCLR');
	for (size_t i = 7; i < narc_get_file_count(narc); i=i+2)
	{
		char outfile[256] = "";
		sprintf(outfile, "%s/%d", OUTDIR, (i-7)/2);
		struct NCGR *ncgr = narc_load_file(narc, i);
		//assert(nitro_get_magic(ncgr) == (magic_t)'NCGR');

		//ncer_dump(ncer, NULL);

		struct dim dim;
		ncgr_get_dim(ncgr, &dim);

		//warn("ncer.dim = {.width=%u, .height=%u}", dim.width, dim.height);

		struct image image = {
			.palette = nclr_get_palette(nclr, 0),
			//.pixels = buffer_alloc(32*24),
			//.dim = {24,32},
			.pixels = buffer_alloc(32*64),
			.dim = {32,64},
		};

		struct coords offset = {16, 8};
		struct coords offset2 = {16, 32};
		//ncer_draw_cell(ncer, 0, ncgr, &image, offset);
		//ncer_draw_cell(ncer, 2, ncgr, &image, offset2);

		image.pixels = ncgr_get_pixels(ncgr);
			if (image.pixels == NULL) {
				warn("Error ripping %s.", outfile);
				continue;
			}
		image.palette = nclr_get_palette(nclr, 0);
		
		write_sprite(&image, outfile);
		
		nitro_free(ncgr);
		FREE(ncgr);

	}
	nitro_free(ncer);
	FREE(ncer);
	nitro_free(nclr);
	FREE(nclr);

	exit(EXIT_SUCCESS);
}

static void
bw2rip_icon(void)
{
	#define FILENAME "./Resources/Narcs/poke_icon-w.narc"
	#define OUTDIR "./Out/pokeIcons"

	MKDIR("");

	struct NARC *narc = open_narc(FILENAME);

	struct NCER *ncer = narc_load_file(narc, 3);
	//assert(nitro_get_magic(ncer) == (magic_t)'NCER');
	
	struct NCLR *nclr = narc_load_file(narc, 0);
	//assert(nitro_get_magic(nclr) == (magic_t)'NCLR');
	for (size_t i = 8; i < narc_get_file_count(narc); i=i+2)
	{
		char outfile[256] = "";
		sprintf(outfile, "%s/%d", OUTDIR, (i-7)/2);
		struct NCGR *ncgr = narc_load_file(narc, i);
		//assert(nitro_get_magic(ncgr) == (magic_t)'NCGR');

		//ncer_dump(ncer, NULL);

		struct dim dim;
		ncgr_get_dim(ncgr, &dim);

		//warn("ncer.dim = {.width=%u, .height=%u}", dim.width, dim.height);

		struct image image = {
			.palette = nclr_get_palette(nclr, 0),
			//.pixels = buffer_alloc(32*24),
			//.dim = {24,32},
			.pixels = buffer_alloc(32*64),
			.dim = {32,64},
		};

		struct coords offset = {16, 8};
		struct coords offset2 = {16, 32};
		//ncer_draw_cell(ncer, 0, ncgr, &image, offset);
		//ncer_draw_cell(ncer, 2, ncgr, &image, offset2);

		image.pixels = ncgr_get_pixels(ncgr);
			if (image.pixels == NULL) {
				warn("Error ripping %s.", outfile);
				continue;
			}
		image.palette = nclr_get_palette(nclr, 0);
		
		write_sprite(&image, outfile);
		
		nitro_free(ncgr);
		FREE(ncgr);

	}
	nitro_free(ncer);
	FREE(ncer);
	nitro_free(nclr);
	FREE(nclr);

	exit(EXIT_SUCCESS);
}

static void
rip_item_icon(void)
{
	#define FILENAME "./Resources/Narcs/item_icon.narc"
	#define OUTDIR "./Out/itemIcons"

	MKDIR("");

	struct NARC *narc = open_narc(FILENAME);

	struct NCER *ncer = narc_load_file(narc, 1);
	assert(nitro_get_magic(ncer) == (magic_t)'NCER');
	
	struct NCLR *nclr = narc_load_file(narc, 3);
	assert(nitro_get_magic(nclr) == (magic_t)'NCLR');

	struct NCGR *ncgr = narc_load_file(narc, 2);
	assert(nitro_get_magic(ncgr) == (magic_t)'NCGR');

	//open next file,
	//check if image.
	//if image, recursively check next files for palette and save image with it
	//stop when next file is image, repeat

	//void *test = narc_load_file(narc, i);
	//	if (nitro_get_magic(test) == (magic_t)'NCGR'){
	//		printf("test");
	int item = 0;
	for (size_t i = 2; i < narc_get_file_count(narc); i++)
	{
		char outfile[256] = "";
		sprintf(outfile, "%s/%d", OUTDIR, item);

		void *test = narc_load_file(narc, i);
		if (nitro_get_magic(test) == (magic_t)'NCGR'){
			ncgr = test;
		}
		else if (nitro_get_magic(test) == (magic_t)'NCLR'){
			printf("%d \n", item);
			item++;
			nclr = test;
			//ncer_dump(ncer, NULL);

			struct dim dim;
			ncgr_get_dim(ncgr, &dim);

			//warn("ncer.dim = {.width=%u, .height=%u}", dim.width, dim.height);

			struct image image = {
				.palette = nclr_get_palette(nclr, 0),
				.pixels = buffer_alloc(32*32),
				.dim = {32,32},
			};

			struct coords offset = {16, 16};
			ncer_draw_cell(ncer, 0, ncgr, &image, offset);
			
			write_sprite(&image, outfile);
		}
	}
	
	nitro_free(ncgr);
	FREE(ncgr);
	nitro_free(ncer);
	FREE(ncer);
	nitro_free(nclr);
	FREE(nclr);

	exit(EXIT_SUCCESS);
}

static void
dump_ncer(void)
{
	struct NCER *ncer = open_nitro("venu.ncer", 'NCER');

	ncer_dump(ncer, NULL);

	exit(EXIT_SUCCESS);
}

static void
render_ncer(void)
{
	struct NCER *ncer = open_nitro("venu.ncer", 'NCER');
	struct NCGR *ncgr = open_nitro("venu-parts.ncgr", 'NCGR');
	struct NCLR *nclr = open_nitro("venu.nclr", 'NCLR');

	struct image image = {};

	image.dim = (struct dim){96, 96};
	image.pixels = buffer_alloc(96 * 96);
	image.palette = nclr_get_palette(nclr, 0);

	if (image.palette == NULL || image.pixels == NULL) {
		warn("error");
		exit(EXIT_FAILURE);
	}

	nitro_free(nclr);
	FREE(nclr);

	/*
	printf("ncer.magic = %s\n", STRMAGIC(ncer->header.magic));
	printf("ncer.size = %u\n", ncer->header.size);
	printf("ncer.cebk.cell_count = %u\n", ncer->cebk.header.cell_count);
	printf("ncer.cebk.cell_type = %u\n", ncer->cebk.header.cell_type);
	*/

	int status = OKAY;
	const int i = 2;
	struct coords offset = {.x = 48, .y = 48};

	if (ncer_draw_cell(ncer, i, ncgr, &image, offset)) {
		warn("error drawing cell %d; bailing", i);
		status = FAIL;
		goto cleanup;
	}
	/* if (ncer_draw_boxes(ncer, i, &image, offset)) {
		warn("error drawing boxes for cell %d; bailing", i);
		status = FAIL;
		goto cleanup;
	} */

	FILE *fp = fopen("out.png", "wb");
	if (fp == NULL) {
		warn("Could not open \"out.png\"");
		status = FAIL;
		goto cleanup;
	}
	if (image_write_png(&image, fp)) {
		warn("Error writing image");
		status = FAIL;
		goto cleanup;
	}


	cleanup:

	FREE(image.pixels);
	FREE(image.palette->colors);
	FREE(image.palette);

	nitro_free(ncgr);
	nitro_free(ncer);

	FREE(ncgr);
	FREE(ncer);
	exit(EXIT_SUCCESS);
}

/******************************************************************************/

int
main(int argc, char *argv[])
{
	UNUSED(argc);
	//list();
	//rip_sprites();
	//rip_bw_sprites();
	//rip_bw_trainers();
	//rip_trainers();
	//rip_trainers2();
	//rip_footprint();
	//rip_icon();
	//bwrip_icon();
	//dump_ncer();
	//render_ncer();
	int i;
	sscanf(argv[1], "%d", &i);
	switch(i) 
	{ 
		case 1: 
			rip_sprites();
			break;
		case 2: 
			rip_bw_sprites();
			break;
		case 3: 
			rip_bw_trainers();
			break;
		case 4: 
			rip_trainers();
			break;
		case 5: 
			rip_trainers2();
			break;
		case 6: 
			rip_footprint();
			break;
		case 7: 
			rip_icon();
			break;
		case 8: 
			bwrip_icon();
			break;
		case 9: 
			bw2rip_icon();
			break;
		case 10: 
			rip_item_icon();
			break;
		case 11: 
			rip_textbox_dppt();
			break;
		case 12: 
			rip_textbox_hgss();
			break;
		case 13: 
			rip_trainer();
			break;
		case 14: 
			rip_bw_beta_sprites();
			break;
		default:
			list();
			break;
	} 
}

//todo: add item icon ripping.
//expose nitro format lookup so you can:
