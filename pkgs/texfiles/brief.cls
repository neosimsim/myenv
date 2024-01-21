 %                      Version 2, December 2004
 %
 %   Copyright (C) 2015 Ben Nasrallah, Alexander <alexander.nasrallah@gmail.com>
 %
 %   Everyone is permitted to copy and distribute verbatim or modified
 %   copies of this license document, and changing it is allowed as long
 %   as the name is changed.
 %
 %              DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
 %     TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
 %
 %    0. You just DO WHAT THE FUCK YOU WANT TO.
 %%

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{brief}[]

\DeclareOption*{%
  \PassOptionsToClass{\CurrentOption}{article}%
}
\ProcessOptions\relax
\LoadClass{article}

\RequirePackage[left=2.4cm, top=2.0cm, right=2.0cm, bottom=2.0cm, columnsep=1.25cm]{geometry}
\RequirePackage[none]{hyphenat}

%%%%%%%%%%%%%
% Variables %
%%%%%%%%%%%%%
\newcommand{\Email}{}
\newcommand{\email}[1]{\renewcommand{\Email}{#1}}
\newcommand{\Address}{}
\newcommand{\address}[1]{\renewcommand{\Address}{#1}}
\newcommand{\DeliveryAdress}{}
\newcommand{\deliveryAdress}[1]{\renewcommand{\DeliveryAdress}{#1}}
\newcommand{\Subject}{}
\newcommand{\subject}[1]{\renewcommand{\Subject}{#1}}

\RequirePackage{xcolor}
\definecolor{darkgray}{HTML}{444444}

\setlength{\parindent}{0em}
\setlength{\parskip}{1eM}

\RequirePackage[ocgcolorlinks]{hyperref}
\AtBeginDocument{%
	\thispagestyle{firstpage}
	\hypersetup{
		hidelinks,
		linkcolor=darkgray,
		urlcolor=darkgray,
		pdfauthor={\@author},
		pdftitle={\@title} -- {\@author},
	}
	\rule{0cm}{0cm}
	\vspace{1.5cm}
	\begin{flushleft}
		{\scriptsize \@author, \Address}

		\DeliveryAdress{}
	\end{flushleft}
	\begin{flushright}
		\@date
	\end{flushright}
	\textbf{\Subject{}}

	\vspace{1eM}
	\raggedright
}

\RequirePackage{fancyhdr, lastpage}
\pagestyle{fancy}
\fancyhf{}
\lhead{\textcolor{darkgray}{\@author}}
\rhead{\textcolor{darkgray}{\href{mailto:\Email}{\Email}}}
\lfoot{\textcolor{darkgray}{\Address}}
\rfoot{\textcolor{darkgray}{Seite \thepage{} von \pageref{LastPage}}}
\fancypagestyle{firstpage}
{
	\lhead{}
}
\renewcommand{\headrulewidth}{0pt}

\endinput
