export interface Territory {
  id: number;
  userId: number;
  name: string;
  instructions: string;
  points: [number, number][];
  created: string;
  updated: string;
}
