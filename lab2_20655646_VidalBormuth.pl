

%Función extra:
 esListaString([]).
 esListaString([H|T]):- string(H), esListaString(T).


%_________________________________________

%TDA fecha: se usa para tener un modelo estandar para todas fechas que se usaran en los demas TDAs.
%Representación: fecha[entero dia, entero mes,entero año]

%Capa constructor:
fecha(D,M,A,F):- 
integer(D), integer(M), integer(A),
D > 0, D < 32, M > 0, M < 13,
F=[D,M,A].


%Capa pertenencia:
esFecha([D,M,A]):- 
integer(D), integer(M), integer(A),
D > 0, D < 32, M > 0, M < 13.

%Capa selector:
getDia([D,M,A],D):- esFecha([D,M,A]).
getMes([D,M,A],M):- esFecha([D,M,A]).
getAno([D,M,A],A):- esFecha([D,M,A]).


%_________________________________________

%TDA recompensa: se usa para representar una recompensa en una pregunta.
%representación: recompensa[string nombre oferto, entero monto].

%Capa constructor:
%recompensaVacia= ["",0];

recompensa(O,MR,R):- 
string(O), integer(MR), 
R= [O,MR].

%Capa pertenencia:
esRecompensa([O,MT]):-
string(O), integer(MT), 
MT>0; ([O,MT]==["",0]).

%Capa selector:
getOfertor([O,MT],O):- esRecompensa([O,MT]).
getMontoRecompensa([O,MT],MT):- esRecompensa([O,MT]).


%_________________________________________

%TDA usuario: representa un usuario y sus elementos obligatorios.
%Representación: usuario[string nombre, string contraseña, entero reputación, lista de string referencias].

%Capa constructor: 
%Entrada: un string NU (nombre del usuario), un string CU (contraseña del usuario), un entero mayor que 0 (reputación del usuario),
%una lista de strings LR (lista de referencias del usuario) y una variable para asignar al usuario.
%Salida: un Usuario (lista de elementos) si las entradas son validas y un false sino.

usuario([]).
crearUsuario(NU,CU,RU,LR, USUARIO):-
string(NU), string(CU), integer(RU), esListaString(LR),
RU > -1,
USUARIO= [NU,CU,RU,LR].

%Ejemplo U1= ["Maria", "Maria1999", 50, ["Racket","c#"]].
%Ejemplo U2= ["Ana","A1234", 70, ["java","python"]].
%Ejemplo U3= ["Juan","juan2000", 20, ["python","c++"]].
%Ejemplo U4= ["Pedro", "P340", 90,  ["python","c++"]].

%Capa pertenencia:
%Entrada: una lista de elementos que sirve como postulante a un usuario.
%Salida: un booleano, un trus si la lista es un usuario y un false sino.

esUsuario([]).
esUsuario([NU,CU,RU,LR]):-
string(NU), string(CU), integer(RU), esListaString(LR),
RU > -1 .

%Capa selector:
%Entrada: todas los predicados de la capa selector reciben un usuario y una variable para guardar el dato buscado.

%Salida: un string, nombre del usuario.
getNameUser([NU,CU,RU,LR],NU):- esUsuario([NU,CU,RU,LR]).

%Entrada: un string, contraseña del usuario.
getPassUser([NU,CU,RU,LR],CU):- esUsuario([NU,CU,RU,LR]).

%Salida: un entero mayor que 0, puntos de reputación del usuario.
getReputUser([NU,CU,RU,LR],RU):- esUsuario([NU,CU,RU,LR]).

%Salida: una lista de strings, una lista de las referencias del usuario.
getRefeUser([NU,CU,RU,LR],LR):- esUsuario([NU,CU,RU,LR]).


%_________________________________________

%TDA lista de usuarios: representa una lista de usuarios en el stack.
%Representación: una lista de usuarios [usuario1, usuario2, ......., usuarioN].

%Capa pertenencia:
%Entrada: una lista con elementos, sirve como postulante para una lista de usuarios.
%Salida: un booleano, un true si la lista corresponde a una lista de usuarios.

esListaUserVacia([]).
esListaUsuarios([]).
esListaUsuarios([H|T]):- esUsuario(H),esListaUsuarios(T).


%Capa modificador:
%Entrada: una lista de usuarios o una nueva lista vacia, el nuevo usuario y una variable para asignar la nueva lista de usuarios con el usuario incluido.
%Salida: un nueva lista de usuarios.

agregarUsuario([], [NU,CU,RU,LR], [[NU,CU,RU,LR]]):- esUsuario([NU,CU,RU,LR]). %caso base
agregarUsuario([H|T], [NU,CU,RU,LR], [H|NEWT]):- 
esListaUsuarios([H|T]),
agregarUsuario(T, [NU,CU,RU,LR], NEWT). 

%Ejemplo: [["Maria", "Maria1999", 50, ["Racket","c#"]],["Ana","A1234", 70, ["java","python"]],["Juan","juan2000", 20, ["python","c++"]]].

%Capa selector: entrega a un usuario en una lista de usuarios al idendificarlo por su nombre.
%Entrada: una lista de usuarios, un string NOMBRE (nombre del usuario que se desea obtener) y una variable U para asignar al usuario.
%Salida: un usuario.
getUsuario([H|_], NOMBRE, U):- 
getNameUser(H,NU), NU == NOMBRE, U= H.

getUsuario([_|T], NOMBRE, U):-
getUsuario(T, NOMBRE, U).

%Función extra: verifica si existe un usuario en la lista de usuarios.
existeUsuario([H|_], NOMBRE):- 
getNameUser(H,NU), NU == NOMBRE, !.

existeUsuario([_|T], NOMBRE):-
existeUsuario(T, NOMBRE).


%_________________________________________

%TDA respuesta: representa una respuesta y sus elementos obligatorios.
%Representación: respuesta[entero id respuesta, string autor, fecha fecha de publicación, string contenido respuesta, lista etiquetas,
% string estado [Aceptada/Rechazada], entero votos a favor, entero votos en contra, entero reportes].

%Capa constructor:
%Entrada: entero IDR (identificador respuesta), string AR (autor de la respuesta), fecha FP (fecha de publicación), string C (contenido), 
%lista de strings LE (lista de etiquetas), string EA (estado de aceptacion), entero VF (votos a favor), entero VC (votos en contra), 
%entero NR (entero de reportes) y una variable RESPUESTA para asignar la respuesta contruida.
%Salida: un Respuesta (lista de elementos) si las entradas son correctas y un false sino.

respuesta([]).
crearRespuesta(IDR, AR, FP, C, LE, EA, VF, VC, NR, RESPUESTA):-
integer(IDR), string(AR), esFecha(FP), string(C), esListaString(LE), string(EA), integer(VF), integer(VC), integer(NR), 
IDR > -1, VF > -1, VC > -1, NR > -1,
RESPUESTA= [IDR, AR, FP, C, LE, EA, VF, VC, NR].

%Ejemplo R1= [0,"Pedro", [2,3,2020], "Aumenta la complejidad y puede generar resultados impredecibles", ["Variables globales", "Problemas"], "Aceptada",5,2,0].

%Capa pertenencia:
%Entrada: una lista de elementos que sirve como postulante a ser una respuesta.
%Salida: un booleano, un true si la lista es una respuesta y un false sino.

esRespuesta([]).
esRespuesta([IDR, AR, FP, C, LE, EA, VF, VC, NR]):-
integer(IDR), string(AR), esFecha(FP), string(C), esListaString(LE), string(EA), integer(VF), integer(VC), integer(NR),
IDR > -1, VF > -1, VC > -1, NR > -1 .

%Capa selector:
%Entrada: todas los predicados de la capa selector reciben una respuesta y una variable para guardar el dato buscado.

%Salida: un entero, ID de la respuesta.
getIdRes([IDR, AR, FP, C, LE, EA, VF, VC, NR],IDR):- esRespuesta([IDR, AR, FP, C, LE, EA, VF, VC, NR]).

%Salida: un string, nombre del autor de la respuesta.
getAutorRes([IDR, AR, FP, C, LE, EA, VF, VC, NR],AR):- esRespuesta([IDR, AR, FP, C, LE, EA, VF, VC, NR]).

%Salida: una fecha, fecha de publicación de la respuesta.
getFechaRes([IDR, AR, FP, C, LE, EA, VF, VC, NR],FP):- esRespuesta([IDR, AR, FP, C, LE, EA, VF, VC, NR]).

%Salida: un string, contenido de la respuesta.
getContenidoRes([IDR, AR, FP, C, LE, EA, VF, VC, NR],C):- esRespuesta([IDR, AR, FP, C, LE, EA, VF, VC, NR]).

%Salida: un lista de strings, lista de etiquetas de la respuesta.
getListaDeEtiquetasRes([IDR, AR, FP, C, LE, EA, VF, VC, NR],LE):- esRespuesta([IDR, AR, FP, C, LE, EA, VF, VC, NR]).

%Salida: un string, estado de aceptación de la respuesta.
getEstadoRes([IDR, AR, FP, C, LE, EA, VF, VC, NR],EA):- esRespuesta([IDR, AR, FP, C, LE, EA, VF, VC, NR]).

%Salida: un entero, votos a favor de la respuesta.
getVotosAFavorRes([IDR, AR, FP, C, LE, EA, VF, VC, NR],VF):- esRespuesta([IDR, AR, FP, C, LE, EA, VF, VC, NR]).

%Salida: un entero, votos en contra de la respuesta.
getVotosEnContraRes([IDR, AR, FP, C, LE, EA, VF, VC, NR],VC):- esRespuesta([IDR, AR, FP, C, LE, EA, VF, VC, NR]).

%Salida: un entero, reportes de la respuesta.
getReportesRes([IDR, AR, FP, C, LE, EA, VF, VC, NR],NR):- esRespuesta([IDR, AR, FP, C, LE, EA, VF, VC, NR]).

%_________________________________________

%TDA lista de respuestas: representa una lista de respuestas en una pregunta.
%Representación: una lista de respuestas [respuesta1, respuesta2, ......., respuestaN].

%Capa pertenencia:
%Entrada: una lista con elementos, sirve como postulante para una lista de respuestas.
%Salida: un booleano, un true si la lista corresponde a una lista de respuestas y un false sino.
esListaRespuestasVacia([]).
esListaRespuestas([]).
esListaRespuestas([H|T]):- esRespuesta(H),esListaRespuestas(T).

%Capa constructor:
%Entrada: una lista de respuestas o una nueva lista vacia, la nueva respuesta y una variable para asignar la nueva lista de respuestas con la respuesta incluida.
%Salida: una nueva lista de respuestas.

agregarRespuesta([], [IDR, AR, FP, C, LE, EA, VF, VC, NR], [[IDR, AR, FP, C, LE, EA, VF, VC, NR]]):- 
esRespuesta([IDR, AR, FP, C, LE, EA, VF, VC, NR]). %caso base

agregarRespuesta([H|T], [IDR, AR, FP, C, LE, EA, VF, VC, NR], [H|NEWT]):- 
esListaRespuestas([H|T]),
agregarRespuesta(T, [IDR, AR, FP, C, LE, EA, VF, VC, NR], NEWT). 

%Ejemplo: [[0,"Pedro", [2,3,2020], "Aumenta la complejidad y puede generar resultados impredecibles", ["Variables globales", "Problemas"], "Aceptada",5,2,0],[1, "Juan", [1, 3, 2020], "El problema de las variables globales es que crea dependencias ocultas. Cuando se trata de aplicaciones grandes, ni tú mismo sabes/recuerdas/tienes claro los objetos que tienes y sus relaciones.", ["Malas practicas","errores"], "", 2, 9, 1],[2, "Maria", [13, 11, 2020], "Usando Qt Style Sheet", [], "Rechazada", 6, 3, 0]].

%Capa selector: entrega una respuesta en una lista de respuesta al idendificarla por su id.
%Entrada: una lista de respuestas, un entero IDR (identificador de la respuesta que se desea obtener) y una variable R para asignar la respuesta.
%Salida: una respuestas.

getRespuesta([H|_], IDR, R):- 
getIdRes(H,ID), ID == IDR, R= H.

getRespuesta([_|T], IDR, R):-
getRespuesta(T, IDR, R).


%_________________________________________

%TDA pregunta: representa una preguntas y sus lementos obligatorios.
%Representación: pregunta[entero id pregunta, string nombre del autor, fecha fecha de publicación, string contenido, lista de strings etiquetas,
%string estado pregunta [Abierta/Cerrada], entero número de visualizaciones, entero votos a favor, entero votos en contra, recompensa Recompensa,
% entero reportes, lista de respuestas respuestas].

%Capa constructor:
%Entrada: un entero ID (identificador de pregunta), un string AP (nombre del autor de la pregunta), una fecha FP (fecha de publicación de la pregunta), 
%un string C (contenido pregunta), una lista de strins LE (listas de etiquetas), un string EP (estado de la pregunta), un entero NV (número de visualizaciones),
%un entero VF (votos a favor), un entero VC (votos en contra), una recompensa REC (recompensa con ofertor y monto), una lista de respuestas RESPUESTAS 
%(repuestas de la pregunta), una variable PREGUNTA para asignar la pregunta construida. 
%Salida: una Pregunta (lista de elementos) si las entradas son correctas y un false sino. 

pregunta([]).
crearPregunta(IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS,PREGUNTA):-
integer(IDP),string(AP),esFecha(FP),string(C),esListaString(LE),string(EP),integer(NV),integer(VF),integer(VC),esRecompensa(REC),integer(NR),esListaRespuestas(RESPUESTAS),
IDP > -1, NV > -1, VF > -1, VC > -1, NR > -1,
PREGUNTA= [IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS].

%Ejemplo R1= [0, "Maria", [29, 2, 2020], "¿Por qué es considerado una mala práctica utilizar variables globales?,¿Realmente son perjudiciales?", ["Malas practicas","variables globales"], "Abierta", 10, 3, 2, ["Maria", 10], 1, [[0,"Pedro", [2,3,2020], "Aumenta la complejidad y puede generar resultados impredecibles", ["Variables globales", "Problemas"], "Aceptada",5,2,0]]].

%Capa pertenencia:
%Entrada: una lista de elementos que sirve como postulante a ser una pregunta.
%Salida: un booleano, un true si la lista es una pregunta y un false sino.

esPregunta([]).
esPregunta([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]):-
integer(IDP),string(AP),esFecha(FP),string(C),esListaString(LE),string(EP),integer(NV),integer(VF),integer(VC),esRecompensa(REC),integer(NR),esListaRespuestas(RESPUESTAS),
IDP > -1, NV > -1, VF > -1, VC > -1, NR > -1 .

%Capa selector:
%Entrada: todos los predicados de la capa selector reciben como entrada una pregunta.

%Salida: un entero, ID de la pregunta.
getIdPreg([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS],IDP):- esPregunta([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]).

%Salida: un string, nombre del autor de la pregunta.
getAutorPreg([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS],AP):- esPregunta([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]).

%Salida: una fecha, fecha de publicación de la pregunta.
getIdFechaPreg([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS],FP):- esPregunta([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]).

%Salida: un string, contenido de la pregunta.
getContenidoPreg([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS],C):- esPregunta([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]).

%Salida: una lista de strings, lista de etiquetas de la pregunta.
getEtiquetasPreg([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS],LE):- esPregunta([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]).

%Salida: un string, estado de la pregunta.
getEstadoPreg([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS],EP):- esPregunta([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]).

%Salida: un entero, número de visualizaciones de la pregunta.
getVisualizaciones([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS],NV):- esPregunta([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]).

%Salida: un entero, votos a favor de la pregunta.
getVotosAFavorPreg([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS],VF):- esPregunta([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]).

%Salida: un entero, votos en contra de la pregunta.
getVotosEnContraPreg([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS],VC):- esPregunta([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]).

%Salida: una recompensa, recompensa ofrecida por la pregunta.
getRecompensaPreg([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS],REC):- esPregunta([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]).

%Salida: un entero, número de reportes de la pregunta.
getReportesPreg([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS],NR):- esPregunta([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]).

%Salida: una lista de respuestas, respuestas de la pregunta.
getRespuestasPreg([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS],RESPUESTAS):- esPregunta([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]).


%_________________________________________

%TDA lista de preguntas: representa una lista de preguntas en el stack.
%Representación: una lista de preguntas [pregunta1, pregunta2, ......., preguntaN].

%Capa pertenencia:
%Entrada: una lista con elementos, sirve como postulante para una lista de preguntas.
%Salida: un booleano, un true si la lista corresponde a una lista de preguntas y un false sino.
esListaPreguntasVacia([]).
esListaPreguntas([]).
esListaPreguntas([H|T]):- esPregunta(H),esListaPreguntas(T).

%Capa constructor:
%Entrada: una lista de respuestas o una nueva lista vacia, la nueva respuesta y una variable para asignar la nueva lista de respuestas con la respuesta incluida.
%Salida: una nueva lista de respuestas.

agregarPregunta([], [IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS], [[IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]]):-
esPregunta([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]). 

agregarPregunta([H|T], [IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS],  [H|NEWT]):- 
esListaPreguntas([H|T]),
agregarPregunta(T, [IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS], NEWT). 

%Capa selector: entrega a una pregunta en una lista de preguntas al idendificarla por su id.
%Entrada: una lista de preguntas, un entero IDP (identificador de la pregunta que se desea obtener) y una variable P para asignar la pregunta.
%Salida: un pregunta.

getPregunta([H|_], IDP, P):- 
getIdPreg(H,ID), ID == IDP, P= H.

getPregunta([_|T], IDP, P):-
getPregunta(T, IDP, P).


 %_________________________________________

%TDA stack: representa a Stack Overflow.
%Representación: stack [usuario Sesión activa, lista de usuarios Registrados, lista de preguntas Preguntas stack, entero Correlativo pregunta, entero Correlativo respuestas].

%Capa constructor:
%Entrada: un usuario UA (usuario activo en el stack), una lista de usuarios LU (usuarios registrados en el stack), una lista de preguntas LP (preguntas del stack),
% un entero CP (correlativo para llevar la cuenta de la cantidad de preguntas en el stack), un entero CR (correlativo para llevar la cuenta de la cantidad de respuestas en el stack).
%Salida: un stack.

%stack([[],[],[],0,0]).
crearStack(UA, LU, LP, CP, CR, STACK):-
esUsuario(UA),esListaUsuarios(LU),esListaPreguntas(LP),integer(CP),integer(CR),
CP > -1 , CR > -1,
STACK= [UA, LU, LP, CP, CR].

%Capa pertenencia:
%Entrada: un lista de elementos que sirven como postulante a ser un stack.
%salida: un boolenao, un true si la lista es un stack y un false sino.

esStack([[],[],[],0,0]).
esStack([UA, LU, LP, CP, CR]):-
esUsuario(UA),esListaUsuarios(LU),esListaPreguntas(LP),integer(CP),integer(CR),
CP > -1 , CR > -1.

%Capa selector:
%Entrada: todos los predicados de la capa selector reciben un stack.

%Salida: un usuario, usuario activo en el stack.
getActivo([UA, LU, LP, CP, CR],UA):- esStack([UA, LU, LP, CP, CR]).

%Salida: una lista de usuarios, registrados en el stack.
getListaUsuarios([UA, LU, LP, CP, CR],LU):- esStack([UA, LU, LP, CP, CR]).

%Salida: una lista de preguntas, preguntas del stack.
getListaPreguntas([UA, LU, LP, CP, CR],LP):- esStack([UA, LU, LP, CP, CR]).

%Salida: un entero, correlativo de preguntas en el stack.
getCorrelativoPreg([UA, LU, LP, CP, CR],CP):- esStack([UA, LU, LP, CP, CR]).

%Salida: un entero, correlativo de respuestas en el stack.
getCorrelativoRes([UA, LU, LP, CP, CR],CR):- esStack([UA, LU, LP, CP, CR]).

%_________________________________________
%Desarrollo requerimiento 2: Hechos.

usuario(["Maria", "Maria1999", 50, ["Racket","c#"]]).
usuario(["Ana","A1234", 70, ["java","python"]]).
usuario(["Juan","juan2000", 20, ["python","c++"]]).
usuario(["Pedro", "P340", 90,  ["python","c++"]]).

stack([[],[["Maria", "Maria1999", 50, ["Racket","c#"]],["Ana","A1234", 70, ["java","python"]],["Juan","juan2000", 20, ["python","c++"]]], 
			[[0, "Maria", [29, 2, 2020], "¿Por que es considerado una mala practica utilizar variables globales?,¿Realmente son perjudiciales?", 
			["Malas practicas","variables globales"], "Abierta", 10, 3, 2, ["Maria", 10], 1, 
 			[[0,"Pedro", [2,3,2020], "Aumenta la complejidad y puede generar resultados impredecibles", ["Variables globales", "Problemas"], "Aceptada",5,2,0]]],
			[1, "Ana", [29, 10, 2020], "¿Como poner una imagen de fondo en? Me gustaria saber ¿Como pongo una imagen de fondo a la ventana creada con PyQT5? Muchos me dicen que use Designer, pero estoy evitando usarlo. ¿Conocen alguna manera?", ["python","interfaz-gráfica","imagen"], "Abierta", 20, 5, 2,["",0], 0,
			[[0,"Pedro", [2,3,2020], "Aumenta la complejidad y puede generar resultados impredecibles", ["Variables globales", "Problemas"], "Aceptada",5,2,0],
			[1, "Juan", [1, 3, 2020], "El problema de las variables globales es que crea dependencias ocultas. Cuando se trata de aplicaciones grandes, ni tu mismo sabes/recuerdas/tienes claro los objetos que tienes y sus relaciones.", ["Malas practicas","errores"], "", 2, 9, 1],
 			[2, "Maria", [13, 11, 2020], "Usando Qt Style Sheet", [], "Rechazada", 6, 3, 0]]] ], 
 			10, 12]).

stack([[],[["Maria", "Maria1999", 50, ["Racket","c#"]],["Ana","A1234", 70, ["java","python"]],["Juan","juan2000", 20, ["python","c++"]],["an","H123",0,[]]], 
			[[0, "Maria", [29, 2, 2020], "¿Por que es considerado una mala practica utilizar variables globales?,¿Realmente son perjudiciales?", 
			["Malas practicas","variables globales"], "Abierta", 10, 3, 2, ["Maria", 10], 1, 
 			[[0,"Pedro", [2,3,2020], "Aumenta la complejidad y puede generar resultados impredecibles", ["Variables globales", "Problemas"], "Aceptada",5,2,0]]],
			[1, "Ana", [29, 10, 2020], "¿Como poner una imagen de fondo en? Me gustaria saber ¿Como pongo una imagen de fondo a la ventana creada con PyQT5? Muchos me dicen que use Designer, pero estoy evitando usarlo. ¿Conocen alguna manera?", ["python","interfaz-gráfica","imagen"], "Abierta", 20, 5, 2,["",0], 0,
			[[0,"Pedro", [2,3,2020], "Aumenta la complejidad y puede generar resultados impredecibles", ["Variables globales", "Problemas"], "Aceptada",5,2,0],
			[1, "Juan", [1, 3, 2020], "El problema de las variables globales es que crea dependencias ocultas. Cuando se trata de aplicaciones grandes, ni tu mismo sabes/recuerdas/tienes claro los objetos que tienes y sus relaciones.", ["Malas practicas","errores"], "", 2, 9, 1],
 			[2, "Maria", [13, 11, 2020], "Usando Qt Style Sheet", [], "Rechazada", 6, 3, 0]]] ], 
 			10, 12]).






%Desarrollo función register:

register(SK,NEWUSERNAME, PASS, SKS):-
esStack(SK),string(NEWUSERNAME),string(PASS),string(PASS),
getListaUsuarios(SK,LU),(not(existeUsuario(LU,NEWUSERNAME))),
crearUsuario(NEWUSERNAME,PASS,0,[],NEWUSER),agregarUsuario(LU,NEWUSER,NLU),
getActivo(SK,UA),
getListaPreguntas(SK,LP),
getCorrelativoPreg(SK,CP),
getCorrelativoRes(SK,CR),
crearStack(UA, NLU, LP, CP, CR, NSK),
(not(SKS == NSK)), !, var(SKS), SKS= NKS.


esIgual(A,B):- (A == B), !, var(B), B=A. 
