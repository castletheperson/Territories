export interface Territory {
  id: number;
  userId: number;
  name: string;
  instructions: string;
  boundary: [number, number][];
  created: string;
  updated: string;
}
