/* We need to define _GNU_SOURCE because we use the constant NAN. */
#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdarg.h>

/*
 * LOCAL function
 *
 * Returns the decomposition level to which the k-th data sample belongs.
 */
static int find_level(int k, int ell, int n0, int interp_points)
{
	int level = ell; /* num detail levels */
	while (level)
	{
		if (k & 1) return level;
		k >>= 1;
		level--;
	}
	return 0;
}

/*
 * PUBLIC function
 *
 * Returns the number of samples used in the given sparse representation.
 */
int spr_get_samples (double* u, int ell, int n0)
{
	int n = (n0-1) * (1<<ell) + 1;
	int i, j;

	for (i=j=0; i < n; i++) if (!_isnan(u[i])) j++;
	return j;
}

/*
 * LOCAL function
 *
 * This function does the basic interpolation: it computes the value u[k],
 * given the sparse representation stored in u[0..n-1]. It is tacitly assumed
 * that u[k] is not in the sparse decomposition, that is, u[k] is NAN. The
 * number of points used to interpolate is interp_points, and the spacing at
 * the next coarsest level is spc. 
 */
//Paulo-Calcula o valor interpolado no ponto k
//-u é o vetor que estamos a interpolar
//-ell e n0 servem para calcular n é o número de pontos do vetor (de 0 a n-1)
//-k é o ponto central de interpolação pretendida
//-interp_points representa o tipo de interpolação: 2=interpolação binária; 4=interpolação quadrática
//-spc é o intervalo entre os pontos a interpolar
static double interp2(double* u, int ell, int n0, int interp_points, int k, int spc)
{
	int n = (n0-1) * (1 << ell) + 1;
	double v[interp_points];
	double h[interp_points];
	int ix[interp_points];
	// double* v = new double[interp_points];//JPN
	// double* h = new double[interp_points];
	// int* ix = new int[interp_points];
	double r;
	int j;

	//***memset rebenta no R.
	memset(v, 0, sizeof(v));
	memset(h, 0, sizeof(h));
	memset(ix, 0, sizeof(ix));

	/*
	 * The (interior) points to interpolate from are the following:
	 *
	 * if interp_points == 2:    (no boundary problems in this case)
	 *
	 *    ix[0] = k - spc/2;
	 *    ix[1] = k + spc/2;
	 *
	 * if interp_points == 4:    (need to redefine it close to the boundaries)
	 *
	 *    ix[0] = k - 3*spc/2;
	 *    ix[1] = k - spc/2;
	 *    ix[2] = k + spc/2;
	 *    ix[3] = k + 3*spc/2;
	 *
	 * The following loop handles the general case (interp_points even and
	 * greater than two).
	 */
	for (j=1; j <= interp_points/2; j++)
	{
		ix[interp_points/2-j] = k - (2*j-1)*spc/2;
		ix[interp_points/2-1+j] = k + (2*j-1)*spc/2;
	}

	if (interp_points == 2) /* two-point, linear interpolation */
	{
		h[0] = h[1] = 0.5;
	}
	else if (interp_points == 4) /* four-point, cubic interpolation */
	{
		if (ix[0] < 0) /* left boundary */
		{
			ix[0] = 0;
			ix[1] = spc;
			ix[2] = 2*spc;
			ix[3] = 3*spc;
			h[0] =  5/16.0;
			h[1] = 15/16.0;
			h[2] = -5/16.0;
			h[3] =  1/16.0;
		}
		else if (ix[3] > n-1) /* right boundary */
		{
			ix[0] = n-1-3*spc;
			ix[1] = n-1-2*spc;
			ix[2] = n-1-spc;
			ix[3] = n-1;
			h[0] =  1/16.0;
			h[1] = -5/16.0;
			h[2] = 15/16.0;
			h[3] =  5/16.0;
		}
		else /* interior */
		{
			h[0] = -1/16.0;
			h[1] =  9/16.0;
			h[2] =  9/16.0;
			h[3] = -1/16.0;
		}
	}
	else
	{
		printf("Paulo-Valores de ix inferiores a 0: %d, %d, %d, %d; spc=%d; k=%d.\n", ix[0], ix[1], ix[2], ix[3], spc, k);
		exit(-1);
	}

	for (j=0; j < interp_points; j++)
	{
		if (ix[j] < 0 || ix[j] > n-1)
		{
			fprintf(stderr, "ERROR: interp2(): ix[%d]=%d\n", j, ix[j]);
			exit(-1);
		}
		if (_isnan(u[ix[j]])) //JPN (isnan(u[ix[j]]))
		{
			v[j] = interp2(u, ell, n0, interp_points, ix[j],
				1 << (ell - find_level(ix[j], ell, n0, interp_points) + 1));
		}
		else
		{
			v[j] = u[ix[j]];
		}
	}
	for (r=j=0; j < interp_points; j++) r += h[j] * v[j];
	return r;
}

/*
 * PUBLIC function
 *
 * Converts a data vector of n elements to a sparse representation.
 * It sets a data value u[i] in u[0..n-1] to NAN if the absolute value of the
 * corresponding wavelet coefficient d[i] is smaller than epsilon.
 */
 //Paulo-Faz a interpolação, por níveis, do vetor u. Retorna a função u colocando a NAN os pontos que não são necessários para reconstruir a função, onde a curva da função é suave
 //, pontos em que o valor calculado por interpolação dos pontos vizinhos é muito próximo (ou igual) do valor verdadeiro
 //Paulo-Quando um ponto que serviu para calcular a interpolação de outro ponto que foi colocado a NAN, obtem-se esse valor por interpolação desse ponto, e assim sucessivamente
void spr_data_to_sparse (double* u, int u_size, int* ell_p, int* n0_p, int* interp_points_p, double* eps_p)
{
	//JPN Set the array size to the nearest next power of two
	double *u_new;
	int len = u_size;
	len--;
	len |= len >> 1;   // divide by 2^k for consecutive doublings of k up to 32,
	len |= len >> 2;   // and thenor the results.
	len |= len >> 4;
	len |= len >> 8;
	len |= len >> 16;
	len++;       
		printf("[Paulo]u_size ficou %d e new size ficou %d.\n", u_size, len);
	u_new = (double *) malloc(len*sizeof(double));
	memcpy(u_new, u, u_size*sizeof(double));
	int ii;
	for(ii=u_size; ii<len; ii++) {
		 u_new[ii] = 0.0;
	}
	//JPN Set the array size to the nearest next power of two

	int ell; int n0; int interp_points; double eps;
	ell = *ell_p; n0 = *n0_p; interp_points = *interp_points_p; eps = *eps_p;
 
	int n = (n0-1) * (1<<ell) + 1;
		//printf("[Paulo]Valor de n [(n0-1) * (1<<ell) + 1] ficou %d (ell=%d;n0=%d;interppoints=%d;eps=%g).\n", n, ell, n0,interp_points,eps);

	int spc = 1;
	int i, j;

	/* run through the data, from the finest to the coarsest detail levels */
	for (j=ell; j >= 1; j--)
	{
			//printf("[Paulo]Vai iniciar nivel %d.\n", j);
		spc <<= 1; /* the separation between points doubles with each level */
		for (i=spc/2; i < n; i+=spc) /* run through all points at level j */
		{
			if (!_isnan(u_new[i])) //JPN (!isnan(u_new[i]))
			{
				if (fabs(u_new[i] - interp2(u_new, ell, n0, interp_points, i, spc)) < eps)
				{
					u_new[i] = NAN;
					//unsigned long nan[2]={0xffffffff, 0x7fffffff}; //JPN
					//u_new[i] = *( double* )nan;
				}
			}
		}
	}

	//JPN Return the array with the original size
	memcpy(u, u_new, u_size*sizeof(double));
	//JPN Set the array size to the nearest next power of two

	//printf("The sparse form has %d points out of %d\n", spr_get_samples(u, ell, n0), n);
}

static int base_two_log(int n)
{
	int r = 0;
	while ((n >>= 1) != 0) r++;
	return r;
}
/*
 * This function returns the number of decomposition levels, given the
 * initial number of points, and interp_points. How is it computed? At the
 * coarsest level, we need at least p=interp_points samples. Then, at the next
 * level, there will be p+(p-1)=2p-1 samples, then (2p-1)+(2p-2)=4p-3, then
 * (4p-3)+(4p-4)=8p-7, etc. At the finest level l there will be n=2^l (p-1)+1
 * samples, hence (n-1)/(p-1) = 2^l, or l = log2((n-1)/(p-1)).
 */
int get_num_levels(int n, int interp_points)
{
	return base_two_log((n-1)/(interp_points-1));
}

//Paulo-A função com os pontos NAN não será optima para classificar, pois não reduz a quantidade de pontos
//. O que deve ser bom será uma matriz com os pontos de cada nível necessários para recuperar o sinal, e respetivos coeficientes
/*
 * Displays the positions of the important coefficients at each level.
 */
void plot_sparse_form(char* title, double* u, int n, int interp_points)
{
	int spc, num_levels = get_num_levels(n, interp_points);
	int used_per_level; /* number of samples used per level */
	int j, k;

	//FILE* fp = popen("gnuplot -persist >& /dev/null", "w");
	FILE* fp = popen("gnuplot -persist", "w");
	fputs("set data style points\n", fp);
	fputs("set nokey\n", fp);
	fputs("set grid\n", fp);
	fprintf(fp, "set xrange [-1:%d]\n", n);
	fprintf(fp, "set yrange [-0.5:%g]\n", num_levels+0.5);
	fprintf(fp, "set title '%s. Detail levels: %d'\n", title, num_levels);
	/*
	 * The vertical positions 1..num_levels correspond to detail spaces, from
	 * the coarsest to the finer level, and position 0 corresponds to the
	 * coarsest approximation. Depending on the convention, we have, for
	 * example (num_levels=4, that is, four detail levels):
	 *
	 * coarsest ----------------> finest
	 *
	 *   0      1      2      3     4       (position on y axis)
	 *
	 *  V0      W0     W1     W2    W3      (labels, initial space V4=V3+W3)
	 *
	 *  V4      W4     W3     W2    W1      (labels, initial space V0=V1+W1)
	 */

	fputs("set ytics ('V_0' 0,", fp);
	for (j=1; j < num_levels; j++) fprintf(fp, "'W_%d' %d,", j-1, j);
	fprintf(fp, "'W_%d' %d);\n", j-1, j);

	fprintf(fp, "set y2tics ('V_%d' 0,", num_levels);
	for (j=1; j < num_levels; j++) fprintf(fp, "'W_%d' %d,", num_levels-j+1, j);
	fprintf(fp, "'W_%d' %d);\n", num_levels-j+1, j);

	/* first, the unused sample positions, then the used ones */
	fputs("plot '-' w points 19, '-' with points 1\n", fp);

	/* run through the data, from the coarsest to the finest level */
	spc = 1 << num_levels; 
	for (j=1; j <= num_levels; j++)
	{
		/* run through all points at level j */
		for (k=spc/2; k < n; k+=spc)
		{
			if (isnan(u[k]))
			{
				/* u[k] is not available and its approximated value can be found
				 * by interpolating via interp(u, n, k, interp_points, spc) */
				fprintf(fp, "%d\t%d\n", k, j);
			}
		}
		spc >>= 1; /* the separation between points halves with each level */
	}

	fputs("e\n", fp);

	/* run through the data, from the coarsest to the finest level */
	spc = 1 << num_levels;
	for (j=1; j <= num_levels; j++)
	{
		used_per_level = 0; /* we count the used samples in each level */
		/* run through all points at level j */
		for (k=spc/2; k < n; k+=spc)
		{
			if (!isnan(u[k]))
			{
				/* u[k] is available */
				fprintf(fp, "%d\t%d\n", k, j);
				used_per_level++;
			}
		}
		printf(" Level %d, separation %d, %d samples kept\n", j, spc, used_per_level);
		spc >>= 1; /* the separation between points halves with each level */
	}

	/* The detail spaces have been dealt with, but the coarsest approximation
	 * grid still needs to be shown. All the points are known, since this is
	 * not a detail / wavelet space. */
	spc = 1 << num_levels;
	printf(" Coarsest level samples:");
	for (k=0; k < n; k+=spc)
	{
		fprintf(fp, "%d\t0\n", k);
		printf(" %d", k);
	}
	putchar('\n');

	fputs("e\n", fp);
	pclose(fp);
}

/*
 * Converts a sparse representation to a data vector.
 */
//Paulo-Reconstrução do sinal
// void sparse_form_to_data(double* u, int n, int interp_points)
// {
	// int num_levels = get_num_levels(n, interp_points);
	// int spc = 1 << num_levels; /* coarsest level spacing = 2^num_levels */
	// int i, j;

		// char dummy[1024];

	// /* run through the data, from the coarsest to the finest detail levels */
	// for (j=1; j <= num_levels; j++)
	// {
			// printf("Paulo-level %d, interval %d.", j, spc);
		// for (i=spc/2; i < n; i+=spc) /* run through all points at level j */
		// {
			// if (_isnan(u[i]))
			// {
				// u[i] = interp(u, n, i, interp_points, spc);
			// }
			// //printf("Paulo-interp of point %d = %g.", i, u[i]);
			// //fgets(dummy, sizeof(dummy), stdin);
		// }
		// spc >>= 1; /* the separation between points halves with each level */
	// }
// }

